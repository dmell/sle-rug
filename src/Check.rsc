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
TEnv collect(AForm f) {
  return { <q.src, q.id, q.qtext, mapAType(q.ty)> | /AQuestion q <- f.questions, q has id}; 
}

set[Message] check(AForm f, TEnv tenv, UseDef useDef) 
  = ( {} | it + check(q, tenv, useDef) | AQuestion q <- f.questions);

// - produce an error if there are declared questions with the same name but different types.
// - duplicate labels should trigger a warning 
// - the declared type computed questions should match the type of the expression.
set[Message] check(AQuestion q, TEnv tenv, UseDef useDef) 
  = { error("Reference to undefined question", q.src) | /AExpr e <- q, e has name && e.name notin tenv<1>}
  + { error("Condition is not boolean", q.src) | q has condition && typeOf(q.condition, tenv, useDef) != tbool()}
  + { error("Duplicate question with different type", q.src) | q has id  && size((tenv<1,3>)[q.id]) > 1}
  + { warning("Duplicate labels", q.src) | q has qtext && size((tenv<2,1>)[q.qtext]) > 1}
  + { warning("Different labels for the same question", q.src) | q has id && size((tenv<1,2>)[q.id]) > 1};
  //+ ( {} | it + check(exp, tenv, useDef) | /AExpr exp <- q.expr, q has expr)
  //+ ( {} | it + check(exp, tenv, useDef) | /AExpr exp <- q.condition, q has condition);


// Check operand compatibility with operators.
// E.g. for an addition node add(lhs, rhs), 
//   the requirement is that typeOf(lhs) == typeOf(rhs) == tint()
set[Message] check(AExpr e, TEnv tenv, UseDef useDef) {
  set[Message] msgs = {};
  
  switch (e) {
    case ref(str x, src = loc u):
      msgs += { error("Undeclared question", u) | useDef[u] == {} };
    case mul (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() };
    case div (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() };
    case sub (AExpr l , AExpr r, src = loc u):
	  msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() };
    case add (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(l, tenv, useDef) != tint() };
    case not (AExpr e, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(e, tenv, useDef) != tbool()};
    case or (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tbool() || typeOf(l, tenv, useDef) != tbool() };
    case and (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tbool() || typeOf(l, tenv, useDef) != tbool() };
	  case gt (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()};
    case lt (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()};
    case geq (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()};
    case leq (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef) || typeOf(r, tenv, useDef) != tint() || typeOf(r, tenv, useDef) != tint()};
    case equal (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef)};
    case notEqual (AExpr l , AExpr r, src = loc u):
      msgs += { error("Uncompatible types", u) | typeOf(l, tenv, useDef) != typeOf(r, tenv, useDef)};    
    // etc.
  }
  
  return msgs; 
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
    case boolCons(int b, src = loc u):
      return tbool();
    case mul(AExpr l, AExpr r, src = loc u):
      return tint();
    case div(AExpr l, AExpr r, src = loc u):
      return tint();
    case sub(AExpr l, AExpr r, src = loc u):
      return tint();
    case add(AExpr l, AExpr r, src = loc u):
      return tint();
    case not(AExpr e, src = loc u):
      return tbool();
    case and(AExpr l, AExpr r, src = loc u):
      return tbool();
    case or(AExpr l, AExpr r, src = loc u):
      return tbool();
    case gt(AExpr l, AExpr r, src = loc u):
      return tbool();
    case lt(AExpr l, AExpr r, src = loc u):
      return tbool();
    case geq(AExpr l, AExpr r, src = loc u):
      return tbool();
    case leq(AExpr l, AExpr r, src = loc u):
      return tbool();
    case equal(AExpr l, AExpr r, src = loc u):
      return tbool();
    case notEqual(AExpr l, AExpr r, src = loc u):
      return tbool();  
    // etc.
    }
  	return tunknown(); 
}

/* 
 * Pattern-based dispatch style:
 * 
 * Type typeOf(ref(str x, src = loc u), TEnv tenv, UseDef useDef) = t
 *   when <u, loc d> <- useDef, <d, x, _, Type t> <- tenv
 *
 * ... etc.
 * 
 * default Type typeOf(AExpr _, TEnv _, UseDef _) = tunknown();
 *
 */
 
 

