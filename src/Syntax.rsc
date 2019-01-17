module Syntax

extend lang::std::Layout;
extend lang::std::Id;

/*
 * Concrete syntax of QL
 */

start syntax Form 
	= "form" Id "{" Question* "}"; 

// TODO: question, computed question, block, if-then-else, if-then
syntax Question
	= normal: Str Id ":" Type
	| computed: Str Id ":" Type "=" Expr
	| block: "{" Question* "}"
	| ifthen: "if (" Expr ") {" Question* "}"
	| ifthenelse: "if (" Expr ") {" Question* "}" "else" "{" Question* "}"
	; 

// TODO: +, -, *, /, &&, ||, !, >, <, <=, >=, ==, !=, literals (bool, int, str)
// Think about disambiguation using priorities and associativity
// and use C/Java style precedence rules (look it up on the internet)

syntax Expr   = Id \ "true" \ "false" // true/false are reserved keywords.
  | Str
  | Bool
  | Int
  | bracket "(" Expr ")" 
  > left Expr "*" Expr
  > left Expr "/" Expr
  > left (
      left Expr "+" Expr
    | left Expr "-" Expr
  )
  > "!" Expr
  > non-assoc (
    non-assoc Expr "\>" Expr
    | non-assoc Expr "\<" Expr
    | non-assoc Expr "\<=" Expr
	| non-assoc Expr "\>=" Expr
  	| non-assoc Expr "==" Expr
  	| non-assoc Expr "!=" Expr
  )
  > left Expr "&&" Expr
  > left Expr "||" Expr
  ;

syntax Expr 
	= Id \ "true" \ "false" // true/false are reserved keywords.
	| Str
	| Bool
	| Int
	| bracket "(" Expr ")" 
 	> left Expr "*" Expr
 	> left Expr "/" Expr
 	> left (
    	left Expr "+" Expr
    	| left Expr "-" Expr
  	)
	> "!" Expr
		> non-assoc (
	    non-assoc Expr "\>" Expr
	    | non-assoc Expr "\<" Expr
	    | non-assoc Expr "\<=" Expr
		| non-assoc Expr "\>=" Expr
	  	| non-assoc Expr "==" Expr
	  	| non-assoc Expr "!=" Expr
	)
	> left Expr "&&" Expr
	> left Expr "||" Expr
	;

  
syntax Type
	= "integer" | "boolean" | "string";  
 
// lexical => no comments nor space are allowed in such expressions  

  
lexical Str  = "\"" ![\"]* "\"";
lexical Int  = [0-9]+;



lexical Bool
	= "true"
	| "false"
	;
