module Transform

import Syntax;
import Resolve;
import AST;
import List;

import CST2AST;
import IO;
import ParseTree;

/* 
 * Transforming QL forms
 */
 
 
/* Normalization:
 *  wrt to the semantics of QL the following
 *     q0: "" int; 
 *     if (a) { 
 *        if (b) { 
 *          q1: "" int; 
 *        } 
 *        q2: "" int; 
 *      }
 *
 *  is equivalent to
 *     if (true) q0: "" int;
 *     if (a && b) q1: "" int;
 *     if (a) q2: "" int;
 *
 * Write a transformation that performs this flattening transformation.
 *
 */
 
AForm flatten(AForm f) {
	f.questions = flattenQuestion([],f.questions);
	return f; 
}

list[AQuestion] flattenQuestion (list[AExpr] conditions, list[AQuestion] questions){
	list[AQuestion] questionList = [];
	for(AQuestion q <- questions){
		switch(q){
			case question(str qtext, str id, AType ty):{
				questionList += ifThenQuestion(makeAndExpr (conditions + [boolCons(true)]), [question(qtext,id,ty)]);
			}
			case computedQuestion(str qtext, str id, AType ty, AExpr expr):{
				questionList += ifThenQuestion( makeAndExpr (conditions + [boolCons(true)]), [ computedQuestion(qtext,id,ty,expr)]);
			}
			case ifThenQuestion (AExpr condition, list[AQuestion] questions):{
				if(size(questions) == 1){
					
					questionList += ifThenQuestion(makeAndExpr(conditions + [condition]),[questions[0]]);
				} else
					questionList += flattenQuestion (conditions + [condition], questions);
			}
			case ifThenElseQuestion (AExpr condition, list[AQuestion] questions, list[AQuestion] questions2):{
				questionList += flattenQuestion (conditions + [condition], questions) +
								 flattenQuestion (conditions , questions2);
			}
		}
	}
	return questionList;
}
AExpr makeAndExpr (list[AExpr] exs){
	if(size(exs) == 1){
		return exs[0];
	}	
	AExpr condition = exs[0];
		for(AExpr e <- exs[1..] ){
			if(e != boolCons(true)){
				condition = and(condition,e);
			}
		}
	return condition;
}

/* Rename refactoring:
 *
 * Write a refactoring transformation that consistently renames all occurrences of the same name.
 * Use the results of name resolution to find the equivalence class of a name.
 *
 */

start[Form] rename(start[Form] f, loc useOrDef, str newName, UseDef useDef) {
  	Id newId;
  	// check if the new id is valid
  	try {
  		newId = parse(#Id, newName);
  	}
  	catch:{
  		print("Error: Invalid id name\n Returning the same tree with no changes\n");
  		return f;
  	}
	
	ast = cst2ast(f);
  	// check that the name is not in use
  	assert newName notin defs(ast)<0> : "name in use";
  	
  	
	str getOldName (AForm form, loc auxUseOrDef){
	  	visit(form){
	  		case question(str qtext, str id, AType ty, src = loc s) : {
	  			if (s == auxUseOrDef){
	  				return id;
	  			}
	  		}
	  		case computedQuestion(str qtext, str id, AType ty, AExpr expr, src = loc s):{
	  			if (s == auxUOrDef){
	  				return id;
	  			}
	  		}
	  		case ref(str name, src = loc s) : {
	  			if (s == auxUseOrDef){
	  				return name;
	  			}
	  		}			
	  	}
	}

	oldName = getOldName(ast, useOrDef);
  
  	return visit (f) {
    	case (Id)`<Id x>` =>  newId when "<x>" == oldName
  	}
} 