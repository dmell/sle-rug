module CST2AST

import Syntax;
import AST;

import ParseTree;
import String;

/*
 * Implement a mapping from concrete syntax trees (CSTs) to abstract syntax trees (ASTs)
 *
 * - Use switch to do case distinction with concrete patterns (like in Hack your JS) 
 * - Map regular CST arguments (e.g., *, +, ?) to lists 
 *   (NB: you can iterate over * / + arguments using `<-` in comprehensions or for-loops).
 * - Map lexical nodes to Rascal primitive types (bool, int, str)
 * - See the ref example on how to obtain and propagate source locations.
 */

AForm cst2ast(start[Form] sf) {
	//Form f = sf.top; // remove layout before and after form
	//return form("", [], src=f@\loc); 
	return cst2ast(sf.top);
}

AForm cst2ast (f:(Form)`form <Id x> { <Question* ss> }`)
	= form ( "<x>", [cst2ast(s) | Question s <- ss]
		,src = f@\loc);

// for switch-case blocks we don't need the default case because we are working with 
// a predefined grammar

AQuestion cst2ast(Question q) {
	switch (q) {
		case (Question)`<Str s> <Id x> : <Type t>`
			: return question("<s>", "<x>", cst2ast(t), src = x@\loc);
		case (Question)`<Str s> <Id x> : <Type t> = <Expr e>`
			: return computedQuestion("<s>", "<x>", cst2ast(t), cst2ast(e), src = x@\loc);
		case (Question)`{ <Question* ss> }`
			: return block([cst2ast(s) | Question s <- ss], src = q@\loc); 
		case (Question)`if ( <Expr e> ) { <Question* ss> }`
			: return ifThenQuestion(cst2ast(e), [cst2ast (s) | Question s <- ss], src = q@\loc);
		case (Question)`if ( <Expr e> ) { <Question* ss> } else { <Question* xx> }`
			: return ifThenElseQuestion(cst2ast(e), [cst2ast (s) | Question s <- ss], [cst2ast (x) | Question x <- xx], src = q@\loc);
	}
}

AExpr cst2ast(Expr e) {
	switch (e) {
	    case (Expr)`<Id x>`: return ref("<x>", src=e@\loc);
	    case (Expr)`<Str s>`:
	    	return strCons("<s>", src=e@\loc);
	    case (Expr)`<Int i>`:
	    	return intCons(toInt("<i>"), src=e@\loc);
	    case (Expr)`<Bool b>`:
	    	return boolCons (fromString("<b>"), src=e@\loc);
	    case (Expr)`(<Expr ex>)`: // sure? why not `(<Expr ex>)`
	    	return exprCons(cst2ast(ex), src=e@\loc);
	    case (Expr)`!<Expr ex>`:
	    	return not (cst2ast(ex), src=e@\loc);
	    case (Expr)`<Expr ex1> * <Expr ex2>`:
	    	return mul(cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> / <Expr ex2>`:
	    	return div(cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> + <Expr ex2>`:
	    	return add(cst2ast(ex1), cst2ast(ex2), src=e@\loc); 
	    case (Expr)`<Expr ex1> - <Expr ex2>`:
	    	return sub (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> \> <Expr ex2>`:
	    	return gt (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> \< <Expr ex2>`:
	    	return lt (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> \<= <Expr ex2>`:
	    	return leq (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> \>= <Expr ex2>`:
	    	return geq (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> == <Expr ex2>`:
	    	return equal (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> != <Expr ex2>`:
	    	return notEqual (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> && <Expr ex2>`:
	    	return and (cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	    case (Expr)`<Expr ex1> || <Expr ex2>`:
	    	return or(cst2ast(ex1), cst2ast(ex2), src=e@\loc);
	}
}

AType cst2ast(Type t) {
	switch (t) {
	  	case (Type)`boolean`:
	  		return boolean(src=t@\loc);
	  	case (Type)`integer`:
  			return integer(src=t@\loc);
  		case (Type)`string`:
  			return string(src=t@\loc);
  	}
  
}
