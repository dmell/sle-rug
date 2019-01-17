module AST

/*
 * Define Abstract Syntax for QL
 *
 * - complete the following data types
 * - make sure there is an almost one-to-one correspondence with the grammar
 */

data AForm(loc src = |tmp:///|)
	= form(str name, list[AQuestion] questions);

data AQuestion(loc src = |tmp:///|)
	= question(str qtext, str id, AType ty)
	| computedQuestion(str qtext, str id, AType ty, AExpr expr)
	| block(list[AQuestion] questions)
	| ifThenQuestion (AExpr condition, list[AQuestion] questions)
	| ifThenElseQuestion (AExpr condition, list[AQuestion] questions, list[AQuestion] questions2) 
	;


data AExpr(loc src = |tmp:///|)
	= ref (str name)
	| strCons (str s)
	| intCons (int n)
	| boolCons (bool b)
	| exprCons (AExpr e)
	| mul (AExpr l , AExpr r)
	| div (AExpr l , AExpr r)
	| add (AExpr l , AExpr r)
	| sub (AExpr l , AExpr r)
	| and (AExpr l , AExpr r)
	| not (AExpr e)
	| or (AExpr l , AExpr r)
	| gt (AExpr l , AExpr r)
	| lt (AExpr l , AExpr r)
	| geq (AExpr l , AExpr r)
	| leq (AExpr l , AExpr r)
	| equal (AExpr l , AExpr r)
	| notEqual (AExpr l , AExpr r)
	;

data AType(loc src = |tmp:///|)
	= integer()
	| boolean()
	| string();
