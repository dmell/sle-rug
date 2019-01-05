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
TEnv collectVisit (AForm f){
	TEnv envir = {};
	visit(f){
	
		case question(str qtext, str id, AType ty, src = loc s):{
			envir += {<s, id, qtext, mapAType(ty)>};
		}
		
		case computedQuestion(str qtext, str id, AType ty, AExpr_, src = loc s):{
			envir += {<s, id, qtext, mapAType(ty)>};
		}	
	}
	return envir;
} 

TEnv collectDeepMatch (AForm f){
	TEnv envir = {};
	for(q:/question (str qtext, str id, AType ty, src = loc s) := f){
		envir += {<s, id, qtext, mapAType(ty)>};
	};
	for(q:/computedQuestion (str qtext, str id, AType ty, AExpr_, src = loc s) := f){
		envir += {<s, id, qtext, mapAType(ty)>};
	};
	
	return envir;
}

TEnv collect(AForm f) {
  return { <q.src, q.id, q.qtext, mapAType(q.ty)> | /AQuestion q <- f.questions, q has id}; 
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
			+	{ warning("Duplicate labels", l) | size((tenv<2,0>)[q.qtext]) > 1};
		}
		case computedQuestion(str qtext, str id, AType ty, AExpr expr, src = loc l):{
			return { error("Duplicate question with different type", l) | size((tenv<1,3>)[id]) > 1}
			+	{ warning("Duplicate labels", l) | size((tenv<2,0>)[q.qtext]) > 1}
			+	{ error("The declared type computed question does not match the type of the expression", l)
  					| mapAType(ty) != typeOf(expr, tenv, useDef) }  			
  			+   check(expr, tenv, useDef);
		}
		case block(list[AQuestion] questions, src = loc l):{
			return ( {} | it + check(quest, tenv, useDef) | /AQuestion quest <- questions);
		}
		case ifThenQuestion(AExpr condition, list[AQuestion] questions, src = loc l):{
			return ( {} | it + check(quest, tenv, useDef) | /AQuestion quest <- questions)
			+	{error("Condition is not boolean",l) | typeOf(condition, tenv, useDef) != tbool()}
			+	check(condition, tenv, useDef);
		}
		case ifThenElseQuestion(AExpr condition, list[AQuestion] questions, list[AQuestion] questions2, src = loc l):{
			return ( {} | it + check(quest, tenv, useDef) | /AQuestion quest <- questions)
			+  	( {} | it + check(q2, tenv, useDef) | /AQuestion q2 <- questions2)
			+	{error("Condition is not boolean",l) | typeOf(condition, tenv, useDef) != tbool()}
			+  	check(condition, tenv, useDef);
		}
	}
}

// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()

set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  
  switch (e) {
    case ref(str x, src = loc u):
       	return { error("Reference to undefined question", u) | useDef[u] == {} };
    case exprCons(AExpr expr, src = loc u):
     	return check (expr, tenv, useDef);
    case mul (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case div (AExpr l , AExpr r, src = loc u):
       	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case sub (AExpr l , AExpr r, src = loc u):
	  	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case add (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() }
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case not (AExpr e, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(e, tenv, useDef) != tbool()};
    case or (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tbool() || typeOf(l, tenv, useDef) != tbool() }
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case and (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tbool() || typeOf(l, tenv, useDef) != tbool() }
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
	case gt (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case lt (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case geq (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case leq (AExpr l , AExpr r, src = loc u):
     	 return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()}
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case equal (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef)}
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);
    case notEqual (AExpr l , AExpr r, src = loc u):
      	return { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef)}
    			+ check(l,tenv,useDef)
    			+ check(l,tenv,useDef);    
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
    case intCons(int i, src = loc u):
      return tint();
    case boolCons(bool b, src = loc u):
      return tbool();
    case exprCons(AExpr exp, src = loc u):
      return typeOf(exp, tenv, useDef);
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
    	if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    		return tint();
    	else
    		return tunknown();
    }
    case not(AExpr exp, src = loc u):{
    	if(typeOf(exp, tenv, useDef) == tbool())
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
    		return tint();
    	else
    		return tunknown();
    }
    case lt(AExpr l, AExpr r, src = loc u):{
    	if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    		return tint();
    	else
    		return tunknown();
    }
    case geq(AExpr l, AExpr r, src = loc u):{
    	if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    		return tint();
    	else
    		return tunknown();
    }
    case leq(AExpr l, AExpr r, src = loc u):{
    	if(typeOf(l,tenv,useDef) == tint() && typeOf(r,tenv,useDef) == tint())
    		return tint();
    	else
    		return tunknown();
    }
    case equal(AExpr l, AExpr r, src = loc u):{
    	if(typeOf(l,tenv,useDef) == typeOf(r,tenv,useDef))
    		return typeOf(l, tenv, useDef);
    	else
    		return tunknown();
    }
    case notEqual(AExpr l, AExpr r, src = loc u):{
    	if(typeOf(l,tenv,useDef) == typeOf(r,tenv,useDef))
    		return typeOf(l, tenv, useDef);
    	else
    		return tunknown();
    }
    default: tunknown();
  };
  return tunknown();
}
