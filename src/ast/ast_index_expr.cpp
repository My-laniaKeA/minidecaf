/*
 *  Implementation of "IndexExpr".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new IndexExpr node.
 *
 * PARAMETERS:
 *   base_name  - the name of the array
 *   expr_list  - the expresion list in each []
 *   l          - position in the source text
 */
IndexExpr::IndexExpr(std::string base_name, ExprList *idx_list, Location *l) {

    setBasicInfo(INDEX_EXPR, l);

	this->base_name = base_name;
    this->idx_list = idx_list;
	this->dim = NULL;
}

/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void IndexExpr::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void IndexExpr::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    newLine(os);
    for(auto expr : *this->idx_list){
        os << "[" << expr << "]";
        newLine(os);
    }
    decIndent(os);
}