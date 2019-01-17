module Compile

import AST;
import Resolve;
import IO;
import lang::html5::DOM; // see standard library

/*
 * Implement a compiler for QL to HTML and Javascript
 *
 * - assume the form is type- and name-correct
 * - separate the compiler in two parts form2html and form2js producing 2 files
 * - use string templates to generate Javascript
 * - use the HTML5Node type and the `str toString(HTML5Node x)` function to format to string
 * - use any client web framework (e.g. Vue, React, jQuery, whatever) you like for event handling
 * - map booleans to checkboxes, strings to textfields, ints to numeric text fields
 * - be sure to generate uneditable widgets for computed questions!
 * - if needed, use the name analysis to link uses to definitions
 */

void compile(AForm f) {
  	writeFile(f.src[extension="js"].top, form2js(f));
  	writeFile(f.src[extension="html"].top, toString(form2html(f)));
  	writeFile(f.src[extension="css"].top, styleSheet());
}

str styleSheet(){
	return "
	'body{
	'   background: #ffdba6;
	'    font-family: sans-serif;
	'}
	'.main{
	'    border-radius: 25px;
	'    background-color: #ffffff;
	'    padding: 20px;
	'    width: 75%;
	'    margin: auto;
	'    max-width: 600px;
	'}
	'h1{
	'	text-align:center;
	'}
	'select{
	'	width:100%;
	'	height: 30px;
	'}
	'input{
	'	width:100%;
	'}
	'button{
	'	text-align:center;
	'	width: 100%;
	'	margin-top:20px;
	'}
	'.definitionQuestion{
	'    padding: 10px;
	'    background: antiquewhite;
	'    border-radius: 5px;
	'    margin-bottom: 40px;
	'}
	'.computedQuestion{
	'	padding: 10px;
	'    background: #efc792;
	'    border-radius: 5px;
	'    margin-bottom: 40px;
	'}
	'.questionBlock, .ifElse{
	'	padding: 10px;
	'	background: #dfdad3;
	'    border-radius: 5px;
	'	margin-bottom:20px;
	'}
	'.condition_false{
	'	display:none;
	'}";
}

HTML5Node questions2html(list[AQuestion] questions) {
  	return div([question2html(q) | \AQuestion q <- questions]);
}

HTML5Node form2html(AForm f) {
  	return html(
  				head(
  					script(src(f.src[extension="js"].file)),
  					link( \rel("stylesheet"), href(f.src[extension="css"].file) )
  				),
  				body(
  					div(
  						class("main"),
	  					id("form_" + f.name),
	  					h1("Questionnaire <f.name>"),
						questions2html(f.questions),
						button("submit <f.name>",onclick("myFunction()"))		
  					)	
  				)
 		 	);
}

// Transform the inputs into HTML elements
HTML5Node inputType(str idTag,integer()) = input(id(idTag),\type("number"),\value(0),onchange("myFunction()"));
HTML5Node inputType(str idTag,string())	 = input(id(idTag),\type("text"),\value(""),onchange("myFunction()"));
HTML5Node inputType(str idTag,boolean()) = select(
										  		  id(idTag),
												  onchange("myFunction()"),
												  option("False",\value("false")),
												  option("True",\value("true"))
											);

// Generate HTML for normal questions
HTML5Node question2html(question(str label, str idTag, AType t))
  	= div(
  		  class("definitionQuestion"),
  		  p(label),
  		  inputType(idTag, t)
  	  );
  
// Generate HTML for computed questions
HTML5Node question2html(computedQuestion(str label, str idTag, AType t, AExpr _)) {
  return div(class("computedQuestion"),
  			p(label),
  			textarea(id(idTag),html5attr("readonly",""))
  		 );
}
// Generate HTML for bock of questions
HTML5Node question2html(block(list[AQuestion] questions))
  	= div([question2html(q) | \AQuestion q <- questions] + [class("questionBlock")]);

// Generate HTML for if block
HTML5Node question2html(ifThenQuestion(AExpr e, list[AQuestion] questions, src=loc u)) {
  	return div([question2html(q) | \AQuestion q <- questions] + [id("if_<u.begin.line>_<u.begin.column>"),class("condition_false")]);
}

// Generate HTML for if-else block
HTML5Node question2html(ifThenElseQuestion(AExpr e, list[AQuestion] questions, list[AQuestion] questions2, src=loc u)) {
  	HTML5Node ifQuestionsDiv = div([question2html(q) | \AQuestion q <- questions] + [id("if_<u.begin.line>_<u.begin.column>"),class("condition_false")]);
  	HTML5Node elseQuestionsDiv = div([question2html(q) | \AQuestion q <- questions2] + [id("else_<u.begin.line>_<u.begin.column>")]);
 	 return div([class("ifElse"),ifQuestionsDiv, elseQuestionsDiv]);
}

///// JAVASCRIPT /////
str form2js(AForm f) {
  	return "function myFunction(){\n" 
  			+ computeQuestions(f.questions) 
  		 	+ "\n};\n" 
  			+ questions2js(f.questions);
}

str computeQuestions(list[AQuestion] questions){
	str assigns = "";
	for (AQuestion q <- questions){
		if(q has id, q has expr){
			assigns += "\t<q.id> = document.getElementById(\"<q.id>\");
					   '\t<q.id>.value = " + expr2js(q.expr) +";\n";	
		}
		if (q has condition){
			if (q has questions2){
				assigns += "if (<expr2js(q.condition)>){\n"+
				computeQuestions(q.questions) +
				" \tdocument.getElementById(\"if_<q.src.begin.line>_<q.src.begin.column>\").classList.remove(\"condition_false\");
				' \tdocument.getElementById(\"else_<q.src.begin.line>_<q.src.begin.column>\").classList.add(\"condition_false\");
				'}else{" +
				computeQuestions(q.questions2) + 
				" \tdocument.getElementById(\"if_<q.src.begin.line>_<q.src.begin.column>\").classList.add(\"condition_false\");
				' \tdocument.getElementById(\"else_<q.src.begin.line>_<q.src.begin.column>\").classList.remove(\"condition_false\");
				'}";
			}else{
				assigns +="if (<expr2js(q.condition)>){\n"+
				computeQuestions(q.questions) +
				"\tdocument.getElementById(\"if_<q.src.begin.line>_<q.src.begin.column>\").classList.remove(\"condition_false\");
				'}else{
				'document.getElementById(\"if_<q.src.begin.line>_<q.src.begin.column>\").classList.add(\"condition_false\");
				'}";
			}
		}
	}	
	return assigns;
}

str questions2js(list[AQuestion] questions) {
  	str questionscode = "";
  	for(AQuestion q <- questions){
  		questionscode += question2js(q) + "\n";
  	}
  	return questionscode;
}

// Simple questions javascript code
str question2js(question(str qtext, str idTag, AType ty)) {
	str getElem = "";
	switch(ty){
		case integer(): getElem = "document.getElementById(\"<idTag>\").value";
		case boolean():	getElem = "document.getElementById(\"<idTag>\").options[document.getElementById(\"<idTag>\").selectedIndex].value";
		case string() : getElem = "document.getElementById(\"<idTag>\").value";
	}
  return "function get_<idTag>(){
  			'\t return <getElem>;
  		 '} \n";
}

// Computed questions javascript code
str question2js(computedQuestion(str qtext, str idTag, AType ty, AExpr expr)) {
  return "function get_<idTag>(){
  			'\t return "+ expr2js(expr) + ";
  		 '}";			
}

// block questions javascript code
str question2js(block(list[AQuestion] questions)) {
  	str questionscode = "";
  	for(AQuestion q <- questions){
  		questionscode += question2js(q) + "\n";
  	}
  	return questionscode;
}

// If-then questions javascript code
str question2js(ifThenQuestion(AExpr condition, list[AQuestion] questions)) {
  	str questionscode = "";
  	for(AQuestion q <- questions){
  		questionscode +=  question2js(q) + "\n";
  	}
  	return questionscode ;
}

// If-then-else questions javascript code
str question2js(ifThenElseQuestion(AExpr condition, list[AQuestion] questions, list[AQuestion] questions2)) {
  	str questionscode = "";
  	
  	for(AQuestion q <- questions){
  		questionscode += question2js(q) + "\n";
  	}
  	
  	for(AQuestion q2 <- questions2){
  		questionscode += question2js(q2) + "\n";
  	}
  	
  	return questionscode;
}

// Convert QL expressions to Javascript expressions
str expr2js(ref(str name)) 		= " this.get_<name>() ";
str expr2js(intCons(int n)) 	= " <n> ";
str expr2js(boolCons(bool b)) 	= " <b> ";
str expr2js(strCons(str s)) 	= " <s> ";
str expr2js(exprCons(AExpr e)) 	= "( " + expr2js(e) + " )"; 

// *
str expr2js(mul(AExpr l, AExpr r)) = expr2js(l) + "*" + expr2js(l);
// /
str expr2js(div(AExpr l, AExpr r)) = expr2js(l) + "/" + expr2js(r);
// +
str expr2js(add(AExpr l, AExpr r)) = expr2js(l) + "+" + expr2js(r);
// -
str expr2js(sub(AExpr l, AExpr r)) = expr2js(l) + "-" + expr2js(r);
// <
str expr2js(lt(AExpr l, AExpr r)) = expr2js(l) + "\<" + expr2js(r);
// <=
str expr2js(leq(AExpr l, AExpr r)) = expr2js(l) + "\<=" + expr2js(r);
// >
str expr2js(gt(AExpr l, AExpr r)) = expr2js(l) + "\>" + expr2js(r);
// >=
str expr2js(geq(AExpr l, AExpr r)) = expr2js(l) + "\>=" + expr2js(r);
// Negation
str expr2js(not(AExpr e)) = "!" + expr2js(e);  
// Or
str expr2js(or(AExpr l, AExpr r)) = expr2js(l) + " || " + expr2js(r);
// And
str expr2js(and(AExpr l, AExpr r)) = expr2js(l) + " && " + expr2js(r);
// equal
str expr2js(equal(AExpr l, AExpr r)) = expr2js(l) + "==" + expr2js(r); 
// Not equal
str expr2js(notEqual(AExpr l, AExpr r)) = expr2js(l) + "!=" + expr2js(r); 
 



