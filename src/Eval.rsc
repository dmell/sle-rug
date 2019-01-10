module Eval

import AST;
import Resolve;

/*
 * Implement big-step semantics for QL
 */
 
// NB: Eval may assume the form is type- and name-correct.


// Semantic domain for expressions (values)
data Value
  = vint(int n)
  | vbool(bool b)
  | vstr(str s)
  ;

// The value environment
alias VEnv = map[str name, Value \value];

// Modeling user input
data Input
  = input(str question, Value \value);
  
// produce an environment which for each question has a default value
// (e.g. 0 for int, "" for str etc.)
VEnv initialEnv(AForm f) {
	return (q.id: vbool(false) | /AQuestion q <- f.questions, q has ty && q.ty == boolean(src = q.ty.src))
		 + (q.id: vint(0) | /AQuestion q <- f.questions, q has ty && q.ty == integer(src = q.ty.src)) 
		 + (q.id: vstr("") | /AQuestion q <- f.questions, q has ty && q.ty == string(src = q.ty.src));
}

VEnv initialEnv2(AForm f) {
	VEnv envir = ();
	visit(f){
	
		case question(str qtext, str id, AType ty, src = loc s):{
			if (ty == boolean(src = ty.src)) envir += (id : vbool(false));
			if (ty == integer(src = ty.src)) envir += (id : vint(0));
			if (ty == string(src = ty.src))  envir += (id : vstr(""));
		}
		
		case computedQuestion(str qtext, str id, AType ty, AExpr_, src = loc s):{
			if (ty == integer(src = ty.src)) envir += (id : vint(0));
			if (ty == boolean(src = ty.src)) envir += (id : vbool(false));
			if (ty == string(src = ty.src))  envir += (id : vstr(""));
		}	
	}
	return envir;
}

// Because of out-of-order use and declaration of questions
// we use the solve primitive in Rascal to find the fixpoint of venv.
VEnv eval(AForm f, Input inp, VEnv venv) {
  return solve (venv) {
    venv = evalOnce(f, inp, venv);
  }
}

VEnv evalOnce(AForm f, Input inp, VEnv venv) {
  return ( () | it + eval(q, venv) | /AQuestion q <- f.questions); 
}

VEnv eval(AQuestion q, Input inp, VEnv venv) {
	switch(q){
		case question(str qtext, str id, AType ty):{
			if (id == inp.question){
				return (id : inp.\value);
			}
		}
		case computedQuestion(str qtext, str id, AType ty, AExpr e):{
			return (id : eval(e));
		}
		
		case block(list[AQuestion] questions):{
			return ( () | it + eval(q, venv) | /AQuestion q <- questions);
		}
		
		case block(list[AQuestion] questions):{
			return ( () | it + eval(q, venv) | /AQuestion q <- questions);
		}
		case ifThenQuestion(AExpr condition, list[AQuestion] questions):{
			return ( () | it + eval(q, venv) | /AQuestion q <- f.questions, eval(condition) == vbool(true)); 
		}
		case ifThenElseQuestion(AExpr condition, list[AQuestion] questions, list[AQuestion] questions2):{
			if (eval(condition) == vbool(true)){
				return ( () | it + eval(q, venv) | /AQuestion q <- questions); 
			} else{
				return ( () | it + eval(q, venv) | /AQuestion q <- questions2);
			}
		}
	}
  return (); 
}

Value eval(AExpr e, VEnv venv) {
  switch (e) {
    case ref(str x): return venv[x];
    case strCons(str s): return vstr(s);
    case intCons (int n): return vint(n);
    case boolCons(bool b): return vbool(b);
    case exprCons (AExpr ex): return eval(ex,env);
    case mul (AExpr l, AExpr r):
    	return vint(eval(l,venv).n * eval(r,venv).n);
    case div (AExpr l, AExpr r):
    	return vint(eval(l,venv).n / eval(r,venv).n);
    case add (AExpr l, AExpr r):
    	return vint(eval(l,venv).n + eval(r,venv).n);
    case sub (AExpr l, AExpr r):
    	return vint(eval(l,venv).n - eval(r,venv).n);
    case and (AExpr l, AExpr r):
    	return vbool(eval(l,venv).b && eval(r,venv).b);
    case not (AExpr e):
    	return vbool( !(eval(e,venv).b) );
    case or (AExpr l, AExpr r):
    	return vbool(eval(l,venv).b || eval(r,venv).b);
    case gt (AExpr l, AExpr r):
    	return vbool(eval(l,venv).n > eval(r,venv).n);
    case lt (AExpr l, AExpr r):
    	return vbool(eval(l,venv).n < eval(r,venv).n);
    case geq (AExpr l, AExpr r):
    	return vbool(eval(l,venv).n >= eval(r,venv).n);
    case leq (AExpr l, AExpr r):
    	return vbool(eval(l,venv).n <= eval(r,venv).n);
    case equal (AExpr l, AExpr r):{
    	switch (l){
    		case insCons (int n): {
    			return vbool(n == eval(r,venv).n);
    		}
    		case strCons(str s): {
    			return vbool(s == eval(r,venv).s);
    		}
  			case boolCons(bool b): {
    			return vbool(b == eval(r,venv).b);
    		}
    	}
    }
    case notEqual (AExpr l, AExpr r):{
    	switch (l){
    		case insCons (int n): {
    			return vins(n != eval(r,venv).n);
    		}
    		case strCons(str s): {
    			return vstr(s != eval(r,venv).s);
    		}
  			case boolCons(bool b): {
    			return vbool(b != eval(r,venv).b);
    		}
    	}
    }
    // etc.
    
    default: throw "Unsupported expression <e>";
  }
}