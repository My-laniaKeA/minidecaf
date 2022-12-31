/*****************************************************
 *  Implementation of the third translation pass.
 *
 *  In the third pass, we will:
 *    translate all the statements and expressions
 *
 *  Keltin Leung 
 */

#include "translation.hpp"
#include "asm/offset_counter.hpp"
#include "ast/ast.hpp"
#include "compiler.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/tac.hpp"
#include "tac/trans_helper.hpp"
#include "type/type.hpp"

using namespace mind;
using namespace mind::symb;
using namespace mind::tac;
using namespace mind::type;
using namespace mind::assembly;

/* Constructor.
 *
 * PARAMETERS:
 *   helper - the translation helper
 */
Translation::Translation(tac::TransHelper *helper) {
    mind_assert(NULL != helper);

    tr = helper;
}

/* Translating an ast::Program node.
 */
void Translation::visit(ast::Program *p) {
    for (auto it = p->func_and_globals->begin();
         it != p->func_and_globals->end(); ++it)
        (*it)->accept(this);
}

// three sugars for parameter offset management
#define RESET_OFFSET() tr->getOffsetCounter()->reset(OffsetCounter::PARAMETER)
#define NEXT_OFFSET(x) tr->getOffsetCounter()->next(OffsetCounter::PARAMETER, x)

/* Translating an ast::FuncDefn node.
 *
 * NOTE:
 *   call tr->startFunc() before translating the statements and
 *   call tr->endFunc() after all the statements have been translated
 */
void Translation::visit(ast::FuncDefn *f) {
	Function *fun = f->ATTR(sym);

	// attaching function entry label
	fun->attachEntryLabel(tr->getNewEntryLabel(fun));

	// arguments
	int order = 0;
	for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
		auto v = (*it)->ATTR(sym);
		v->setOrder(order++);
		v->attachTemp(tr->getNewTempI4());
	}

	fun->offset = fun->getOrder() * POINTER_SIZE;

	RESET_OFFSET();

	if (!f->forward_decl){
		tr->startFunc(fun);

		// You may process params here, i.e use reg or stack to pass parameters
		for (auto it = f->formals->begin(); it != f->formals->end(); ++it) {
			auto v = (*it)->ATTR(sym);
			tr->genProcessParam(v->getTemp(), v->getOrder());
		}

		// translates statement by statement
		for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it)
			(*it)->accept(this);

		tr->genReturn(tr->genLoadImm4(0)); // Return 0 by default

		tr->endFunc();
	}
	else{
		for (auto it = f->stmts->begin(); it != f->stmts->end(); ++it)
			(*it)->accept(this);
	}
}


/* Translating an ast::CallExpr node.
 */
void Translation::visit(ast::CallExpr *e) {

	// arguments
	for (ast::ExprList::iterator it = e->exprs->begin(); it != e->exprs->end(); ++it){
		(*it)->accept(this);
	}
		
	int order = 0;
	for (ast::ExprList::iterator it = e->exprs->begin(); it != e->exprs->end(); ++it){
		tr->genPassParam((*it)->ATTR(val), order, e->exprs->length()); 
		order++;
	}
	
	order = 0;
	e->ATTR(val) = tr->genCall(e->ATTR(sym)->getEntryLabel(), e->exprs->length());
	for (ast::ExprList::iterator it = e->exprs->begin(); it != e->exprs->end(); ++it){
	 	// tr->genPop((*it)->ATTR(val), order); 
		order++;
	}
}

/* Translating an ast::AssignStmt node.
 *
 * NOTE:
 *   different kinds of Lvalue require different translation
 */
void Translation::visit(ast::AssignExpr *s) {
	s->left->accept(this);
	s->e->accept(this);

    ast::VarRef *ref = (ast::VarRef *)(s->left->lvalue);

    if(ref->ATTR(sym)->isGlobalVar()){
        Temp addr = tr->genLoadSym(ref->var); 
		if(ref->ATTR(lv_kind) == ast::Lvalue::ARRAY_ELE)
            addr = tr->genAdd(addr, ref->idx_expr->ATTR(val));
        tr->genStore(s->e->ATTR(val), addr, 0);
    }
    else{
		if(ref->ATTR(lv_kind) == ast::Lvalue::ARRAY_ELE){
            Temp addr = tr->genAdd(ref->ATTR(sym)->getTemp(), ref->idx_expr->ATTR(val));
            tr->genStore(s->e->ATTR(val), addr, 0);
        }
		else
			tr->genAssign(s->left->ATTR(val), s->e->ATTR(val));
    }
    s->ATTR(val) = s->e->ATTR(val);
}

/* Translating an ast::ExprStmt node.
 */
void Translation::visit(ast::ExprStmt *s) { s->e->accept(this); }

/* Translating an ast::IfStmt node.
 *
 * NOTE:
 *   you don't need to test whether the false_brch is empty
 *   (there's an EmptyStmt as placeholder.)
 */
void Translation::visit(ast::IfStmt *s) {
    Label L1 = tr->getNewLabel(); // entry of the false branch
    Label L2 = tr->getNewLabel(); // exit
    s->condition->accept(this);
    tr->genJumpOnZero(L1, s->condition->ATTR(val));

    s->true_brch->accept(this);
    tr->genJump(L2); // done

    tr->genMarkLabel(L1);
    s->false_brch->accept(this);

    tr->genMarkLabel(L2);
}
/* Translating an ast::WhileStmt node.
 */
void Translation::visit(ast::WhileStmt *s) {
    Label L1 = tr->getNewLabel();
    Label L2 = tr->getNewLabel();

    Label old_break = current_break_label;
	Label old_cont = current_continue_label;
    current_break_label = L2;
	current_continue_label = L1;

    tr->genMarkLabel(L1);
    s->condition->accept(this);
    tr->genJumpOnZero(L2, s->condition->ATTR(val));

    s->loop_body->accept(this);
    tr->genJump(L1);

    tr->genMarkLabel(L2);

    current_break_label = old_break;
	current_continue_label = old_cont;
}

/* Translating an ast::DoWhileStmt node.
 */
void Translation::visit(ast::DoWhileStmt *s) {
    Label L1 = tr->getNewLabel();	// loop begin
    Label L2 = tr->getNewLabel();	// break

    Label old_break = current_break_label;
    current_break_label = L2;

	tr->genMarkLabel(L1);
	s->loop_body->accept(this);
	s->condition->accept(this);
	tr->genJumpOnZero(L2, s->condition->ATTR(val));
	
    tr->genJump(L1);

    tr->genMarkLabel(L2);

    current_break_label = old_break;

}

/* Translating an ast::ForStmt node.
 */
void Translation::visit(ast::ForStmt *s) {
    Label L1 = tr->getNewLabel(); // loop
    Label L2 = tr->getNewLabel(); // update
	Label L3 = tr->getNewLabel(); // exit

    Label old_break = current_break_label;
    current_break_label = L3;

	Label old_cont = current_continue_label;
	current_continue_label = L2;

	if (NULL != s->init)		s->init->accept(this);

    tr->genMarkLabel(L1);
    if (NULL != s->condition)	{
		s->condition->accept(this);
    	tr->genJumpOnZero(L3, s->condition->ATTR(val));
	}

    s->loop_body->accept(this);
	tr->genJump(L2);

	tr->genMarkLabel(L2);
	if (NULL != s->update)		s->update->accept(this);
    tr->genJump(L1);

    tr->genMarkLabel(L3);

    current_break_label = old_break;
	current_continue_label = old_cont;
}

/* Translating an ast::BreakStmt node.
 */
void Translation::visit(ast::BreakStmt *s) { tr->genJump(current_break_label); }

/* Translating an ast::ContStmt node.
 */
void Translation::visit(ast::ContStmt *s) { tr->genJump(current_continue_label); }

/* Translating an ast::CompStmt node.
 */
void Translation::visit(ast::CompStmt *c) {
    // translates statement by statement
    for (auto it = c->stmts->begin(); it != c->stmts->end(); ++it)
        (*it)->accept(this);
}
/* Translating an ast::ReturnStmt node.
 */
void Translation::visit(ast::ReturnStmt *s) {
    s->e->accept(this);
    tr->genReturn(s->e->ATTR(val));
}

/* Translating an ast::IfExpr node.
 */
void Translation::visit(ast::IfExpr *e) {
	e->ATTR(val) = tr->getNewTempI4();	// temp variable for expression result

	Label L1 = tr->getNewLabel(); // entry of the false branch
    Label L2 = tr->getNewLabel(); // exit
    e->condition->accept(this);
    tr->genJumpOnZero(L1, e->condition->ATTR(val));

    e->true_brch->accept(this);
	tr->genAssign(e->ATTR(val), e->true_brch->ATTR(val));
    tr->genJump(L2); // done

    tr->genMarkLabel(L1);
    e->false_brch->accept(this);
	tr->genAssign(e->ATTR(val), e->false_brch->ATTR(val));

    tr->genMarkLabel(L2);
}

/* Translating an ast::AddExpr node.
 */
void Translation::visit(ast::AddExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genAdd(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::SubExpr node.
 */
void Translation::visit(ast::SubExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genSub(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::MulExpr node.
 */
void Translation::visit(ast::MulExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMul(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::DivExpr node.
 */
void Translation::visit(ast::DivExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genDiv(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::ModExpr node.
 */
void Translation::visit(ast::ModExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genMod(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::GrtExpr node.
 */
void Translation::visit(ast::GrtExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGtr(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::LesExpr node.
 */
void Translation::visit(ast::LesExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLes(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::GeqExpr node.
 */
void Translation::visit(ast::GeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genGeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::LeqExpr node.
 */
void Translation::visit(ast::LeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::EquExpr node.
 */
void Translation::visit(ast::EquExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genEqu(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::NeqExpr node.
 */
void Translation::visit(ast::NeqExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genNeq(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::AndExpr node.
 */
void Translation::visit(ast::AndExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLAnd(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::OrExpr node.
 */
void Translation::visit(ast::OrExpr *e) {
    e->e1->accept(this);
    e->e2->accept(this);

    e->ATTR(val) = tr->genLOr(e->e1->ATTR(val), e->e2->ATTR(val));
}

/* Translating an ast::IntConst node.
 */
void Translation::visit(ast::IntConst *e) {
    e->ATTR(val) = tr->genLoadImm4(e->value);
}

/* Translating an ast::NegExpr node.
 */
void Translation::visit(ast::NegExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genNeg(e->e->ATTR(val));
}

/* Translating an ast::NotExpr node.
 */
void Translation::visit(ast::NotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genLNot(e->e->ATTR(val));
}

/* Translating an ast::BitNotExpr node.
 */
void Translation::visit(ast::BitNotExpr *e) {
    e->e->accept(this);

    e->ATTR(val) = tr->genBNot(e->e->ATTR(val));
}

/* Translating an ast::LvalueExpr node.
 *
 * NOTE:
 *   different Lvalue kinds need different translation
 */
void Translation::visit(ast::LvalueExpr *e) {
	((ast::VarRef*)e->lvalue)->accept(this);
   	ast::VarRef *ref = (ast::VarRef *)e->lvalue;
	if (ref->ATTR(sym)->isGlobalVar()){
		Temp addr = tr->genLoadSym(ref->ATTR(sym)->getName());
		if (ref->ATTR(type)->isBaseType()) {
			if(ref->ATTR(lv_kind) == ast::Lvalue::ARRAY_ELE)
				addr = tr->genAdd(addr, ref->idx_expr->ATTR(val));
			e->ATTR(val) = tr->genLoad(addr, 0);
		}
		else {
			e->ATTR(val) = addr;
		}
	}
    else {
		if (ref->ATTR(lv_kind) == ast::Lvalue::ARRAY_ELE) {
            Temp temp = tr->genAdd(ref->ATTR(sym)->getTemp(), ref->idx_expr->ATTR(val));
            e->ATTR(val) = tr->genLoad(temp, 0);
        }
		else
			e->ATTR(val) = ref->ATTR(sym)->getTemp();
	}
}

/* Translating an ast::VarRef node.
 *
 * NOTE:
 *   there are two kinds of variable reference: member variables or simple
 * variables
 */
void Translation::visit(ast::VarRef *ref) {
    switch (ref->ATTR(lv_kind)) {
    case ast::Lvalue::SIMPLE_VAR:
        // nothing to do
        break;

	case ast::Lvalue::ARRAY_ELE:
        ref->idx_expr->accept(this);
        break;

    default:
        mind_assert(false); // impossible
    }
    // actually it is so simple :-)
}

/* Translating an ast::VarDecl node.
 */
void Translation::visit(ast::VarDecl *decl) {
	Variable *var = decl->ATTR(sym);
	if (decl->ATTR(sym)->isGlobalVar()) {
		if (decl->type->ATTR(type)->isBaseType()) {
			if(decl->init == NULL){
				tr->genGlobalVarible(decl->name, 0, decl->type->ATTR(type)->getSize());
			}
			else {
				assert(decl->init->getKind() == ast::ASTNode::INT_CONST);
				tr->genGlobalVarible(decl->name, ((ast::IntConst *)(decl->init))->value, decl->type->ATTR(type)->getSize());
			}
		}
		else if (decl->type->ATTR(type)->isArrayType()) {
			tr->genGlobalVarible(decl->name, decl->arr_init, decl->type->ATTR(type)->getSize());
		}
		else
			mind_assert(false);
    }
	else {
	
		if (decl->type->ATTR(type)->isArrayType()) {
			Temp addr = tr->genAlloc(decl->type->ATTR(type)->getSize());
			var->attachTemp(addr);
			if (NULL != decl->arr_init){
				int length = 1;
				for (auto iter = decl->dim->begin(); iter != decl->dim->end(); ++iter)
					length *= (*iter);
				Temp value = tr->genLoadImm4(0);
				tr->genMemset(addr, value, length);
				int offset = 0;
				for (auto iter = decl->arr_init->begin(); iter != decl->arr_init->end(); ++iter) {
					Temp init_value = tr->genLoadImm4((*iter));
					tr->genStore(init_value, addr, offset);
					offset += 4;
				}
			}
		}
		else {
			var->attachTemp(tr->getNewTempI4());
		}

		if (NULL != decl->init){
			decl->init->accept(this);
			tr->genAssign(var->getTemp(), decl->init->ATTR(val));
		}
	}
}

void Translation::visit(ast::IndexExpr *e){
	mind_assert(e->idx_list->length() == e->dim->length() || e->dim->length() == 0);
	auto idx_iter = e->idx_list->begin();
    auto dim_iter  = e->dim->begin();
	int size_factor = 1;
	Temp offset = tr->genLoadImm4(0);
    for(size_t i = 0; i < e->idx_list->length(); ++idx_iter, ++dim_iter, ++i){
        (*idx_iter)->accept(this);
		Temp index = (*idx_iter)->ATTR(val); 
		Temp factor = tr->genLoadImm4(size_factor);
		index = tr->genMul(index, factor);
		offset = tr->genAdd(offset, index);    
		size_factor *= (*dim_iter);
       
	}
    Temp t = tr->genLoadImm4(4);
    offset = tr->genMul(offset, t);
    e->ATTR(val) = offset;
}

/* Translates an entire AST into a Piece list.
 *
 * PARAMETERS:
 *   tree  - the AST
 * RETURNS:
 *   the result Piece list (represented by the first node)
 */
Piece *MindCompiler::translate(ast::Program *tree) {
    TransHelper *helper = new TransHelper(md);

    tree->accept(new Translation(helper));

    return helper->getPiece();
}
