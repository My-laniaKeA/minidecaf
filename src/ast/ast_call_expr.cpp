/*****************************************************
 *  Implementation of "CallExpr".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Minjia Wang
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new CallExpr node.
*
* PARAMETERS:
*   func_name_	- function name
*   exprs_     	- the expresion list as the parameters
*   l       	- position in the source text
*/
CallExpr::CallExpr(std::string func_name_, ExprList* exprs_, Location *l) {
	setBasicInfo(CALL_EXPR, l);

	func_name = func_name_;
	exprs = exprs_;
}


/* Visits the current node.
*
* PARAMETERS:
*   v       - the visitor
*/
void CallExpr::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
*
* PARAMETERS:
*   os      - the output stream
*/
void CallExpr::dumpTo(std::ostream &os) {
	ASTNode::dumpTo(os);
	newLine(os);
	os << func_name;

	newLine(os);
	os << "(" << exprs << ")";
	decIndent(os);
}
