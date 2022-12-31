/*****************************************************
 *  Implementation of "ForStmt".
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

/* Creates a new ForStmt node.
 *
 * PARAMETERS:
 *   _init	 - the initialization expression
 *   _cond   - the test expression
 *   _upd	 - the update expression
 *   _body   - the loop body
 *   _l      - position in the source text
 */
ForStmt::ForStmt(Expr *_init, Expr *_cond, Expr *_upd, Statement *_body,  Location *_l) {
    setBasicInfo(FOR_STMT, _l);

    init = (ASTNode*) _init;
	condition = _cond;
	update = _upd;
    loop_body = _body;
}

/* Creates a new ForStmt node.
 *
 * PARAMETERS:
 *   _init	 - the initialization statement
 *   _cond   - the test expression
 *   _upd	 - the update expression
 *   _body   - the loop body
 *   _l      - position in the source text
 */
ForStmt::ForStmt(Statement *_init, Expr *_cond, Expr *_upd, Statement *_body,  Location *_l) {
	setBasicInfo(FOR_STMT, _l);

	init = (ASTNode*) _init;
	condition = _cond;
	update = _upd;
    loop_body = _body;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void ForStmt::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void ForStmt::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    os << init << " " << condition << " " << update;

    newLine(os);
    os << loop_body << ")";
    decIndent(os);
}
