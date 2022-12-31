/*****************************************************
 *  Implementation of "VarDecl".
 *
 *  Please refer to ast/ast.hpp for the definition.
 *
 *  Keltin Leung 
 */

#include "ast/ast.hpp"
#include "ast/visitor.hpp"
#include "config.hpp"

using namespace mind;
using namespace mind::ast;

/* Creates a new VarDecl node.
 *
 * PARAMETERS:
 *   n       - name of the variable
 *   t       - type of the variable
 *   l       - position in the source text
 */
VarDecl::VarDecl(std::string n, Type *t, Location *l) {

    setBasicInfo(VAR_DECL, l);

    name = n;
    type = t;
	dim = NULL;
    
	init = NULL;
	arr_init = NULL;	
}

VarDecl::VarDecl(std::string n, Type *t, Expr *i, Location *l) {
    setBasicInfo(VAR_DECL, l);

    name = n;
    type = t;
	dim = NULL;
    
	init = i;
	arr_init = NULL;	
}

VarDecl::VarDecl(std::string n, Type *t, DimList *d, Location *l) {

    setBasicInfo(VAR_DECL, l);

    name = n;
    type = t;
	dim = d;
    
	init = NULL;
	arr_init = NULL;   
}

VarDecl::VarDecl(std::string n, Type *t, DimList *d, DimList *arr_init, Location *l) {

    setBasicInfo(VAR_DECL, l);

    name = n;
    type = t;
	dim = d;

	init = NULL;
	this->arr_init = arr_init;
}


/* Visits the current node.
 *
 * PARAMETERS:
 *   v       - the visitor
 */
void VarDecl::accept(Visitor *v) { v->visit(this); }

/* Prints the current AST node.
 *
 * PARAMETERS:
 *   os      - the output stream
 */
void VarDecl::dumpTo(std::ostream &os) {
    ASTNode::dumpTo(os);
    if (init == NULL) {
        os << " " << '"' << name << '"' << " " << type << ")";
    } else {
        os << " " << '"' << name << '"' << " " << type << "=";
        newLine(os);
        os << init << ")";
    }
    decIndent(os);
}