module Check

import AST;
import Resolve;
import Message; // see standard library
import Set; // for errors

data Type
  	= tint()
	| tbool()
  	| tstr()
  	| tunknown()
  	;

// the type environment consisting of defined questions in the form 
alias TEnv = rel[loc def, str name, str label, Type \type];

Type mapAType(AType at)
{
 	switch (at) {
   		case integer(): return tint();
    	case boolean(): return tbool();
    	case string(): return tstr();
  	}
}

// To avoid recursively traversing the form, use the `visit` construct
// or deep match (e.g., `for (/question(...) := f) {...}` )
TEnv collect (AForm f){
	TEnv envir = {};
	visit(f){
	
		case question(str qtext, str id, AType ty, src = loc s) : {
			envir += {<s, id, qtext, mapAType(ty)>};
		}
		
		case computedQuestion(str qtext, str id, AType ty, AExpr_, src = loc s):{
			envir += {<s, id, qtext, mapAType(ty)>};
		}	
	}
	return envir;
} 


set[Message] check(AForm f, TEnv tenv, UseDef useDef) 
	= ( {} | it + check(q, tenv, useDef) | /AQuestion q <- f.questions);

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef){
	switch (q) {
		case question(str qtext, str id, AType ty, src = loc l): {
			return  { error("Duplicate question with different type", l) | size((tenv<1,3>)[id]) > 1}
			+	{ warning("Duplicate labels", l) | size((tenv<2,0>)[q.qtext]) > 1}
			+	{ warning("Different label for occurrences of the same question", l) | size((tenv<1,2>)[q.id]) > 1};
		}
		case computedQuestion(str qtext, str id, AType ty, AExpr expr, src = loc l):{
			return { error("Duplicate question with different type", l) | size((tenv<1,3>)[id]) > 1}
			+	{ warning("Duplicate labels", l) | size((tenv<2,0>)[q.qtext]) > 1}
			+	{ warning("Different label for ocurrences of the same question", l) | size((tenv<1,2>)[q.id]) > 1}
			+	{ error("The declared type computed question does not match the type of the expression", l)
  					| mapAType(ty) != typeOf(expr, tenv, useDef) && typeOf(expr, tenv, useDef) != tunknown() }  			
  			+   check(expr, tenv, useDef);
		}
		case block(list[AQuestion] questions, src = loc l):{
			return ( {} | it + check(q, tenv, useDef) | /AQuestion q <- questions);
		}
		case ifThenQuestion(AExpr condition, list[AQuestion] questions, src = loc l):{
			return ( {} | it + check(q, tenv, useDef) | /AQuestion q <- questions)
			+	{error("Condition is not boolean",l) | typeOf(condition, tenv, useDef) != tbool()}
			+	check(condition, tenv, useDef);
		}
		case ifThenElseQuestion(AExpr condition, list[AQuestion] questions, list[AQuestion] questions2):{
			return ( {} | it + check(q, tenv, useDef) | /AQuestion q <- questions)
			+  	( {} | it + check(q2, tenv, useDef) | /AQuestion q2 <- questions2)
			+	{error("Condition is not boolean",l) | typeOf(condition, tenv, useDef) != tbool()}
			+  	check(condition, tenv, useDef);
		}
	}
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
// the requirement is that typeOf(lhs) == typeOf(rhs) == tint()

set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  	switch (e) {
    	case ref(str x, src = loc u):
       		return { error("Reference to undefined question", u) | useDef[u] == {} };
    	case exprCons(AExpr expr, src = loc u):
     		return check (expr, tenv, useDef);
    	case mul (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case div (AExpr l , AExpr r, src = loc u):
       		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case sub (AExpr l , AExpr r, src = loc u):
	  		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case add (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case not (AExpr e, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(e, tenv, useDef) != tbool()};
    	case or (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tbool() || typeOf(l, tenv, useDef) != tbool() }
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case and (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tbool() || typeOf(l, tenv, useDef) != tbool() }
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
		case gt (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case lt (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case geq (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case leq (AExpr l , AExpr r, src = loc u):
     		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case equal (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef)}
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef);
    	case notEqual (AExpr l , AExpr r, src = loc u):
      		return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef)}
    			+ check(l,tenv,useDef)
    			+ check(r,tenv,useDef); 
    	default: return {};
 	}
 	
}

Type typeOf(AExpr e, TEnv tenv, UseDef useDef) {
  	switch (e) {
    	case ref(str x, src = loc u):  
      		if (<u, loc d> <- useDef, <d, x, _, Type t> <- tenv) {
        		return t;
      		}
    	case strCons(str s, src = loc u):
      		return tstr();
    	case exprCons (AExpr ex):
    		return typeOf(ex,tenv,useDef);
    	case intCons(int i, src = loc u):
      		return tint();
    	case boolCons(bool b, src = loc u):
      		return tbool();
    	case exprCons(AExpr e, src = loc u):
      		return typeOf(e, tenv, useDef);
    	case mul(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tint();
    		else
    			return tunknown();
    	}     
    	case div(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tint();
    		else
    			return tunknown();
    	}
    	case sub(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tint();
    		else
    			return tunknown();
    	}
    	case add(AExpr l, AExpr r, src = loc u):{
    		return (typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint()) ? tint() : tunknown();
    	}
    	case not(AExpr e, src = loc u):{
    		if(typeOf(e, tenv, useDef) == tbool())
    			return tbool();
    		else
    			return tunknown();
    	}    
    	case and(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l, tenv, useDef) == tbool() && typeOf(r, tenv, useDef) == tbool())
    			return tbool();
    		else
    			return tunknown();
    	}    
    	case or(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l, tenv, useDef) == tbool() && typeOf(r, tenv, useDef) == tbool())
    			return tbool();
    		else
    			return tunknown();
   		}    
    	case gt(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tbool();
    		else
    			return tunknown();
    	}
    	case lt(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tbool();
    		else
    			return tunknown();
   		}
    	case geq(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tbool();
    		else
    			return tunknown();
    	}
    	case leq(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    			return tbool();
    		else
    			return tunknown();
    	}
    	case equal(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == typeOf(r,tenv,useDef))
    			return tbool();
    		else
    			return tunknown();
    	}
    	case notEqual(AExpr l, AExpr r, src = loc u):{
    		if(typeOf(l,tenv,useDef) == typeOf(r,tenv,useDef))
    			return tbool();
    		else
    			return tunknown();
   	 	}
    	default: return tunknown();
  	}; 
  	return tunknown();
}

