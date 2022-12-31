/*****************************************************
 *  Implementation of the "TransHelper" class.
 *
 */

#include "tac/trans_helper.hpp"
#include "asm/mach_desc.hpp"
#include "asm/offset_counter.hpp"
#include "config.hpp"
#include "scope/scope.hpp"
#include "scope/scope_stack.hpp"
#include "symb/symbol.hpp"
#include "tac/tac.hpp"

#include <cstdio>
#include <cstring>
#include <sstream>

using namespace mind;
using namespace mind::tac;
using namespace mind::symb;
using namespace mind::scope;
using namespace mind::assembly;

/* Appends a Tac node to the current TAC list.
 *
 * PARAMETERS:
 *   t    - the Tac node
 * NOTE:
 *   a private function
 */
void TransHelper::chainUp(Tac *t) {
    t->prev = tacs_tail;

    if (NULL == tacs)
        tacs_tail = tacs = t;
    else
        tacs_tail = tacs_tail->next = t;
}

/******************** public methods *********************/

/* Constructs a tranlsation helper.
 *
 */
TransHelper::TransHelper(MachineDesc *md) {
    mind_assert(NULL != md);

    mach = md;
    ptail = &head;
    head.next = NULL;
    tacs = tacs_tail = NULL;
    current = NULL;
    var_count = label_count = 0;
    startup_ok = false;
}

/* Gets the offset counter of the target machine.
 *
 * RETURNS:
 *   the offset counter of the target machine
 */
OffsetCounter *TransHelper::getOffsetCounter(void) {
    return mach->getOffsetCounter();
}

/* Allocates a new temporary int32 variable.
 *
 * RETURNS:
 *   the newly created temporary variable
 * NOTE:
 *   we use the variable index to avoid name crash.
 */
Temp TransHelper::getNewTempI4(void) {
    Temp v = new TempObject();
    v->id = var_count++;
    v->size = 4;
    v->offset = 0;
    v->is_offset_fixed = false;

    return v;
}

/* Allocates a new label.
 *
 * RETURNS:
 *   the newly created label
 * NOTE:
 *   we use the label index to avoid name crash.
 */
Label TransHelper::getNewLabel(void) {
    Label l = new LabelObject();
    l->id = label_count++;
    l->str_form = std::string(); // 0 means using the default label name
    l->target = false;

    return l;
}

/* Allocates space on stack for array.
 *
 * RETURNS:
 *   the start address of allocated space
 *
 */
Temp TransHelper::genAlloc(int size){
	Temp v = this->getNewTempI4();
	chainUp(Tac::Alloc(v, size));
	return v;
}

/* Set memory to a specific value.
 *
 * PARAMETERS:
 *   start_addr - the start address for memory
 * 	 value		- the value to be set
 * 	 length		- the size of memory (in words)
 */
void TransHelper::genMemset(Temp start_addr, Temp value, int length){
	chainUp(Tac::Memset(start_addr, value, length));
}

/* Creates a new Label object for the function entry.
 *
 * PARAMETERS:
 *   fn   - the function (symb::Function)
 * RETURNS:
 *   the entry label object
 */
Label TransHelper::getNewEntryLabel(Function *fn) {
    mind_assert(NULL != fn);
    std::string fn_name = fn->getName();

    Label l = new LabelObject();
    l->id = label_count++;
    l->str_form = fn_name;
    l->target = true; // such label is referenced by virtual tables

    return l;
}

/* Creates a Memo tac about the function parameters.
 *
 * PARAMETERS:
 *   f - a Function object
 * RETURNS:
 *   the Memo tac
 * NOTE:
 *   Memo is not operating Tac, i.e. it serves as comments.
 *   this function also fix the offset of the temp vars of the parameters.
 */
Tac *TransHelper::memoOf(Function *f) {
    std::ostringstream oss;
    
    int length = oss.str().size();
    char *memo = new char[length + 1];
    oss.str().copy(memo, length);

    return (Tac::Memo(memo));
}

/* Starts the procession of a Function object.
 *
 * PARAMETERS:
 *   f    - the Function object
 * NOTE:
 *   the newly created Functy object will be chained up into the Piece list
 */
void TransHelper::startFunc(Function *f) {
    mind_assert(NULL != f && NULL == current); // non-reentrant

    Label entry = f->getEntryLabel();
    mind_assert(NULL != entry); // it should have been created in TransPass1

    ptail = ptail->next = new Piece();
    ptail->kind = Piece::FUNCTY;
    ptail->as.functy = new FunctyObject();
    ptail->as.functy->entry = entry;
    current = f;

    // generates entry label
    genMarkLabel(entry);
}

/* Ends the procession of the current Function object.
 *
 * NOTE:
 *   the newly created Functy object will be attached to the Function object
 */
void TransHelper::endFunc(void) {
    // does things automatically
    ptail->as.functy->code = tacs;
    tacs = tacs_tail = NULL;
    current->attachFuncty(ptail->as.functy);
    current = NULL;
}

/* Generates a GlobalVar object.
 */
void TransHelper::genGlobalVarible(std::string name, int value, int size) {
    ptail = ptail->next = new Piece();
    ptail->kind = Piece::GLOBAL;
    ptail->as.globalVar = new GlobalObject();
    ptail->as.globalVar->name = name;
    ptail->as.globalVar->value = value;
	ptail->as.globalVar->arr_value = NULL;
    ptail->as.globalVar->size = size;
}

/* Generates a GlobalVar object.
 */
void TransHelper::genGlobalVarible(std::string name, ast::DimList *value, int size) {
    ptail = ptail->next = new Piece();
    ptail->kind = Piece::GLOBAL;
    ptail->as.globalVar = new GlobalObject();
    ptail->as.globalVar->name = name;
    ptail->as.globalVar->value = 0;
	ptail->as.globalVar->arr_value = value;
    ptail->as.globalVar->size = size;
}

/* Appends an Add tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a + b)
 */
Temp TransHelper::genAdd(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Add(c, a, b));
    return c;
}

/* Appends a Sub tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a - b)
 */
Temp TransHelper::genSub(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Sub(c, a, b));
    return c;
}

/* Appends a Mul tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a * b)
 */
Temp TransHelper::genMul(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Mul(c, a, b));
    return c;
}

/* Appends a Div tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a / b)
 */
Temp TransHelper::genDiv(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Div(c, a, b));
    return c;
}

/* Appends a Mod tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a % b)
 */
Temp TransHelper::genMod(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Mod(c, a, b));
    return c;
}

/* Appends a Equ tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a == b)
 */
Temp TransHelper::genEqu(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Equ(c, a, b));
    return c;
}

/* Appends a Neq tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a != b)
 */
Temp TransHelper::genNeq(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Neq(c, a, b));
    return c;
}

/* Appends a Les tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a < b)
 */
Temp TransHelper::genLes(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Les(c, a, b));
    return c;
}

/* Appends a Leq tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a <= b)
 */
Temp TransHelper::genLeq(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Leq(c, a, b));
    return c;
}

/* Appends a Gtr tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a > b)
 */
Temp TransHelper::genGtr(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Gtr(c, a, b));
    return c;
}

/* Appends a Geq tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a >= b)
 */
Temp TransHelper::genGeq(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::Geq(c, a, b));
    return c;
}

/* Appends a Neg tac node to the current list.
 *
 * PARAMETERS:
 *   src  - operand
 * RETURNS:
 *   the temporary containing the result of (-src)
 */
Temp TransHelper::genNeg(Temp src) {
    Temp c = getNewTempI4();
    chainUp(Tac::Neg(c, src));
    return c;
}

/* Appends a LAnd tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a && b)
 */
Temp TransHelper::genLAnd(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::LAnd(c, a, b));
    return c;
}

/* Appends a LOr tac node to the current list.
 *
 * PARAMETERS:
 *   a    - operand 1
 *   b    - operand 2
 * RETURNS:
 *   the temporary containing the result of (a || b)
 */
Temp TransHelper::genLOr(Temp a, Temp b) {
    Temp c = getNewTempI4();
    chainUp(Tac::LOr(c, a, b));
    return c;
}

/* Appends a LNot tac node to the current list.
 *
 * PARAMETERS:
 *   src  - operand
 * RETURNS:
 *   the temporary containing the result of (!src)
 */
Temp TransHelper::genLNot(Temp src) {
    Temp c = getNewTempI4();
    chainUp(Tac::LNot(c, src));
    return c;
}

/* Appends a BNot tac node to the current list.
 *
 * PARAMETERS:
 *   src  - operand
 * RETURNS:
 *   the temporary containing the result of (!src)
 */
Temp TransHelper::genBNot(Temp src) {
    Temp c = getNewTempI4();
    chainUp(Tac::BNot(c, src));
    return c;
}

/* Appends a Pop tac node to the current list.
 *
 * RETURNS:
 *   the temporary containing the result of the newly poped value
 */
void TransHelper::genPop(Temp dest, int order) {
    // Temp c = getNewTempI4();
    chainUp(Tac::Pop(dest, order));
}

/* Appends a PassParam tac node to the current list.
 *
 * PARAMETERS:
 *   src  - the value of the parameter to be passed
 *   order- the order of the parameter in function call
 */
void TransHelper::genPassParam(Temp src, int order, int total_arg_num) {
	 chainUp(Tac::PassParam(src, order, total_arg_num)); 
}

/* Appends a ProcessParam tac node to the current list.
 *
 * PARAMETERS:
 *   src  - the value of the parameter to be processed
 *   order- the order of the parameter in function call
 */
void TransHelper::genProcessParam(Temp src, int order) { chainUp(Tac::ProcessParam(src, order)); }

/* Appends a PushToReg tac node to the current list.
 *
 * PARAMETERS:
 *   src  - the value to be pushed into the register
 */
void TransHelper::genPushToReg(Temp src) { chainUp(Tac::PushToReg(src)); }

/* Appends a PushToStack tac node to the current list.
 *
 * PARAMETERS:
 *   src  - the value to be pushed into the stack
 */
void TransHelper::genPushToStack(Temp src) { chainUp(Tac::PushToStack(src)); }

/* Appends a ClearArgs tac node to the current list.
 */
void TransHelper::genClearArgs(void) { chainUp(Tac::ClearArgs(getNewTempI4())); }


Temp TransHelper::genCall(Label label, int param_num){
	Temp c = getNewTempI4();
    chainUp(Tac::Call(c, label, param_num));
	return c;
}

/* Appends a Jump tac node to the current list.
 *
 * PARAMETERS:
 *   dest - destination label
 * NOTE:
 *   this is unconditional jump
 */
void TransHelper::genJump(Label dest) { chainUp(Tac::Jump(dest)); }

/* Appends a JZero tac node to the current list.
 *
 * PARAMETERS:
 *   dest - destination label
 *   cond - jump condition
 * NOTE:
 *   this is conditional jump
 */
void TransHelper::genJumpOnZero(Label dest, Temp cond) {
    chainUp(Tac::JZero(dest, cond));
}

/* Appends a Return tac node to the current list.
 *
 * PARAMETERS:
 *   value - return value
 * NOTE:
 *   this operation also terminates the current function execution
 */
void TransHelper::genReturn(Temp value) { chainUp(Tac::Return(value)); }

/* Appends an Assign tac node to the current list.
 *
 * PARAMETERS:
 *   dest  - target variable
 *   src   - source variable
 */
void TransHelper::genAssign(Temp dest, Temp src) {
    chainUp(Tac::Assign(dest, src));
}

/* Appends a LoadImm4 tac node to the current list.
 *
 * PARAMETERS:
 *   value - int32 immediate number
 * RETURNS:
 *   the temporary with the immediate number loaded
 */
Temp TransHelper::genLoadImm4(int value) {
    Temp c = getNewTempI4();
    chainUp(Tac::LoadImm4(c, value));
    return c;
}

/* Appends a MarkLabel tac node to the current list.
 *
 * PARAMETERS:
 *   label - Label object
 * NOTE:
 *   "marking a label" means pointing out the location of the label in the tac
 * list
 */
void TransHelper::genMarkLabel(Label label) { chainUp(Tac::Mark(label)); }

/* Appends a Memo tac node to the current list.
 *
 * PARAMETERS:
 *   comment - the comment line of the memo (please use string literal, e.g.
 * "This is comment") NOTE: memorandum can serve as comment in TAC sequence
 */
void TransHelper::genMemo(const char *comment) { chainUp(Tac::Memo(comment)); }

/* Appends a LoadSymbol tac node to the current list.
 *
 * c <- load symbol var_name
 */
Temp TransHelper::genLoadSym(std::string var_name){
	Temp c = getNewTempI4();
    chainUp(Tac::LoadSymbol(c, var_name));
    return c;
}

/* Appends a Load tac node to the current list.
 *
 * dst <- offset(addr)
 */
Temp TransHelper::genLoad(Temp addr, int offset){
	Temp dst = getNewTempI4();
	chainUp(Tac::Load(dst, addr, offset));
	return dst;
}

/* Appends a Store tac node to the current list.
 *
 * offset(addr) <- src
 */
void TransHelper::genStore(Temp src, Temp addr, int offset){
	chainUp(Tac::Store(src, addr, offset));
}

/* Retrieves the entire Piece list.
 *
 * RETURNS:
 *   the Piece list (representing as a single linked list)
 */
Piece *TransHelper::getPiece(void) { return head.next; }
