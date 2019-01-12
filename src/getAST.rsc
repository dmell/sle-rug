module getAST

import Syntax;
import AST;
import CST2AST;
import ParseTree;


AForm astFunc(loc u){
	return cst2ast(parse(#start[Form],u));
}