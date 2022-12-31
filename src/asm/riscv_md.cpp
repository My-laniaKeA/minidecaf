/*****************************************************
 *  Implementation of RiscvDesc.
 *
 */

#include "asm/riscv_md.hpp"
#include "3rdparty/set.hpp"
#include "asm/offset_counter.hpp"
#include "asm/riscv_frame_manager.hpp"
#include "config.hpp"
#include "options.hpp"
#include "scope/scope.hpp"
#include "symb/symbol.hpp"
#include "tac/flow_graph.hpp"
#include "tac/tac.hpp"

#include <cstring>
#include <iomanip>
#include <sstream>

using namespace mind::assembly;
using namespace mind::tac;
using namespace mind::util;
using namespace mind;

// declaration of empty string
#define EMPTY_STR std::string()
#define WORD_SIZE 4
#define NONE 0
#define CALLER 1
#define CALLEE 2

/* Constructor of RiscvReg.
 *
 * PARAMETERS:
 *   reg_name   - name of this register
 *   is_general - whether this register is a general-purpose one
 *   is_caller_saved - whether this register is a caller-saved one
 */
RiscvReg::RiscvReg(const char *reg_name, bool is_general, int reg_saver) {
    name = reg_name;
    dirty = false;
    var = NULL;
    general = is_general;
	saver = reg_saver;
}

/* Constructor of RiscvDesc.
 *
 */
RiscvDesc::RiscvDesc(void) {
    // {GLOBAL, LOCAL, PARAMETER}
    // Actually, we only use the parameter offset counter,
    // other two options are remained for extension
    int start[3] = {0, 0, 0}; 
    int dir[3] = {+1, -1, +1};
    _counter = new OffsetCounter(start, dir);

    // initializes the register vector
    // (we regard all general-purpose registers as caller-saved, which is
    // different from the Riscv specification)
    _reg[RiscvReg::ZERO] = new RiscvReg("zero", false, NONE); // zero
    _reg[RiscvReg::RA] = new RiscvReg("ra", false, CALLER);     // return address
    _reg[RiscvReg::SP] = new RiscvReg("sp", false, CALLEE);     // stack pointer
    _reg[RiscvReg::GP] = new RiscvReg("gp", false, NONE);     // global pointer
    _reg[RiscvReg::TP] = new RiscvReg("tp", false, NONE);     // thread pointer
    _reg[RiscvReg::T0] = new RiscvReg("t0", true, CALLER);
    _reg[RiscvReg::T1] = new RiscvReg("t1", true, CALLER);
    _reg[RiscvReg::T2] = new RiscvReg("t2", true, CALLER);
    _reg[RiscvReg::T3] = new RiscvReg("t3", true, CALLER);
    _reg[RiscvReg::T4] = new RiscvReg("t4", true, CALLER);
    _reg[RiscvReg::T5] = new RiscvReg("t5", true, CALLER);
    _reg[RiscvReg::T6] = new RiscvReg("t6", true, CALLER);
    _reg[RiscvReg::FP] = new RiscvReg("fp", false, CALLEE); // frame pointer
    _reg[RiscvReg::S1] = new RiscvReg("s1", true, CALLEE);
    _reg[RiscvReg::S2] = new RiscvReg("s2", true, CALLEE);
    _reg[RiscvReg::S3] = new RiscvReg("s3", true, CALLEE);
    _reg[RiscvReg::S4] = new RiscvReg("s4", true, CALLEE);
    _reg[RiscvReg::S5] = new RiscvReg("s5", true, CALLEE);
    _reg[RiscvReg::S6] = new RiscvReg("s6", true, CALLEE);
    _reg[RiscvReg::S7] = new RiscvReg("s7", true, CALLEE);
    _reg[RiscvReg::S8] = new RiscvReg("s8", true, CALLEE);
    _reg[RiscvReg::S9] = new RiscvReg("s9", true, CALLEE);
    _reg[RiscvReg::S10] = new RiscvReg("s10", true, CALLEE);
    _reg[RiscvReg::S11] = new RiscvReg("s11", true, CALLEE);
    _reg[RiscvReg::A0] = new RiscvReg("a0", true, CALLER); // argument, return value
    _reg[RiscvReg::A1] = new RiscvReg("a1", true, CALLER); // argument, return value
    _reg[RiscvReg::A2] = new RiscvReg("a2", true, CALLER); // argument
    _reg[RiscvReg::A3] = new RiscvReg("a3", true, CALLER); // argument
    _reg[RiscvReg::A4] = new RiscvReg("a4", true, CALLER); // argument
    _reg[RiscvReg::A5] = new RiscvReg("a5", true, CALLER); // argument
    _reg[RiscvReg::A6] = new RiscvReg("a6", true, CALLER); // argument
    _reg[RiscvReg::A7] = new RiscvReg("a7", true, CALLER); // argument

	for (int i = 0; i < RiscvReg::TOTAL_NUM; i ++)
		_old_reg[i] = new RiscvReg(_reg[i]);

    _lastUsedReg = 0;
    _label_counter = 0;
}

/* Gets the offset counter for this machine.
 *
 * RETURNS:
 *   the offset counter for Riscv
 */
OffsetCounter *RiscvDesc::getOffsetCounter(void) { return _counter; }


/* Translates the given Piece list into assembly code and output.
 *
 * PARAMETERS:
 *   ps    - the Piece list
 *   os    - the output stream
 */
void RiscvDesc::emitPieces(scope::GlobalScope *gscope, Piece *ps,
                           std::ostream &os) {

    _result = &os;
    // output to .data and .bss segment
    std::ostringstream _data, _bss;

    if (Option::getLevel() == Option::ASMGEN) {
        // program preamble
        emit(EMPTY_STR, ".text", NULL);
        emit(EMPTY_STR, ".globl main", NULL);
        emit(EMPTY_STR, ".align 2", NULL);
		emit(EMPTY_STR, NULL, NULL);
    }
    // translates node by node
	Piece * start_ps = ps;

	// .data
	emit(EMPTY_STR, ".data", NULL);
	while (NULL != ps) {
		switch (ps->kind) {
        case Piece::FUNCTY:
            break;

		case Piece::GLOBAL:
			if (ps->as.globalVar->size == 4 && ps->as.globalVar->value) {
				emit(EMPTY_STR, ((std::string)(".global ") + ps->as.globalVar->name).c_str(), NULL);
				emit((ps->as.globalVar->name).c_str(), NULL, NULL);
				emit(EMPTY_STR, ((std::string)(".word ") + std::to_string(ps->as.globalVar->value)).c_str(), NULL);
			}
			else if (NULL != ps->as.globalVar->arr_value) {
				emit(EMPTY_STR, ((std::string)(".global ") + ps->as.globalVar->name).c_str(), NULL);
				emit((ps->as.globalVar->name).c_str(), NULL, NULL);
				for (auto it = ps->as.globalVar->arr_value->begin(); it != ps->as.globalVar->arr_value->end(); ++it) {
					emit(EMPTY_STR, ((std::string)(".word ") + std::to_string((*it))).c_str(), NULL);
				}
				int remain = ps->as.globalVar->size - 4 * ps->as.globalVar->arr_value->length();
				emit(EMPTY_STR, ((std::string)(".zero ") + std::to_string(remain)).c_str(), NULL);
			}
			break;

        default:
            mind_assert(false); // unreachable
            break;
        }
        ps = ps->next;
	}
	emit(EMPTY_STR, NULL, NULL);
	
	// .bss
	emit(EMPTY_STR, ".bss", NULL);	
	ps = start_ps;
	while (NULL != ps) {
		switch (ps->kind) {
        case Piece::FUNCTY:
            break;

		case Piece::GLOBAL:
			if ((ps->as.globalVar->size == 4 && ps->as.globalVar->value == 0) ||
				(ps->as.globalVar->size != 4 && ps->as.globalVar->arr_value == NULL)) {
				emit(EMPTY_STR, ((std::string)(".global ") + ps->as.globalVar->name).c_str(), NULL);
				emit((ps->as.globalVar->name).c_str(), NULL, NULL);
				emit(EMPTY_STR, ((std::string)(".space ") + std::to_string(ps->as.globalVar->size)).c_str(), NULL);
			}
			break;

        default:
            mind_assert(false); // unreachable
            break;
        }
        ps = ps->next;
	}
	emit(EMPTY_STR, NULL, NULL);

    ps = start_ps;
	while (NULL != ps) {
        switch (ps->kind) {
        case Piece::FUNCTY:
            emitFuncty(ps->as.functy);
            break;

		case Piece::GLOBAL:
			break;

        default:
            mind_assert(false); // unreachable
            break;
        }

        ps = ps->next;
    }


}

/* Allocates a new label (for a basic block).
 *
 * RETURNS:
 *   a new label guaranteed to be non-conflict with the existing ones
 */
const char *RiscvDesc::getNewLabel(void) {
    mind_assert(_label_counter < 10000);

    char *buf = new char[10];
    std::sprintf(buf, "__LL%d", _label_counter++);

    return buf;
}

/* Translates a single basic block into Riscv instructions.
 *
 * PARAMETERS:
 *   b     - the basic block to translate
 *   g     - the control-flow graph
 * RETURNS:
 *   the Riscv instruction sequence of this basic block
 */
RiscvInstr *RiscvDesc::prepareSingleChain(BasicBlock *b, FlowGraph *g) {
    RiscvInstr leading;
    int r0;

    _tail = &leading;
    for (Tac *t = b->tac_chain; t != NULL; t = t->next)
        emitTac(t);

    switch (b->end_kind) {
    case BasicBlock::BY_JUMP:
        spillDirtyRegs(b->LiveOut);
        addInstr(RiscvInstr::J, NULL, NULL, NULL, 0,
                 std::string(g->getBlock(b->next[0])->entry_label), NULL);
        // "B" for "branch"
        break;

    case BasicBlock::BY_JZERO:
        r0 = getRegForRead(b->var, 0, b->LiveOut);
        spillDirtyRegs(b->LiveOut);
        // uses "branch if equal to zero" instruction
        addInstr(RiscvInstr::BEQZ, _reg[r0], NULL, NULL, 0,
                 std::string(g->getBlock(b->next[0])->entry_label), NULL);
        addInstr(RiscvInstr::J, NULL, NULL, NULL, 0,
                 std::string(g->getBlock(b->next[1])->entry_label), NULL);
        break;

    case BasicBlock::BY_RETURN:{
        r0 = getRegForRead(b->var, 0, b->LiveOut);
        spillDirtyRegs(b->LiveOut); // just to deattach all temporary variables
        addInstr(RiscvInstr::MOVE, _reg[RiscvReg::A0], _reg[r0], NULL, 0,
                 EMPTY_STR, NULL);
		
		addInstr(RiscvInstr::ADDI, _reg[RiscvReg::FP], _reg[RiscvReg::FP], NULL,
				 44, EMPTY_STR, NULL);
		// recover callee-saved registers
		int cnt = -12;
		for (int i = RiscvReg::S1; i <= RiscvReg::S11; i++){
			if (_reg[i]->saver == CALLEE){
				addInstr(RiscvInstr::LW, _reg[i], _reg[RiscvReg::FP], NULL, 
						 cnt, EMPTY_STR,NULL);
				cnt -= 4;
			}	
		}

        addInstr(RiscvInstr::MOVE, _reg[RiscvReg::SP], _reg[RiscvReg::FP], NULL,
                 0, EMPTY_STR, NULL);
        addInstr(RiscvInstr::LW, _reg[RiscvReg::RA], _reg[RiscvReg::FP], NULL,
                 -4, EMPTY_STR, NULL);
        addInstr(RiscvInstr::LW, _reg[RiscvReg::FP], _reg[RiscvReg::FP], NULL,
                 -8, EMPTY_STR, NULL);
		addInstr(RiscvInstr::RET, NULL, NULL, NULL, 0, EMPTY_STR, NULL);
		}
        break;

    default:
        mind_assert(false); // unreachable
    }
    _tail = NULL;
    return leading.next;
}

/* Translates a single TAC into Riscv instructions (and records the result.
 *
 * PARAMETERS:
 *   t     - the TAC to translate
 * SIDE-EFFECT:
 *   modifies the "_tail" field
 */
void RiscvDesc::emitTac(Tac *t) {
    std::ostringstream oss;
    t->dump(oss);
    addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR, oss.str().c_str() + 4);

    switch (t->op_code) {
    case Tac::LOAD_IMM4:
        emitLoadImm4Tac(t);
        break;

	case Tac::LOAD_SYMBOL:
		emitLoadSymbolTac(t);
		break;

	case Tac::LOAD:
		emitLoadTac(t);
		break;

	case Tac::STORE:
		emitStoreTac(t);
		break;

    case Tac::NEG:
        emitUnaryTac(RiscvInstr::NEG, t);
        break;

	case Tac::BNOT:
        emitUnaryTac(RiscvInstr::NOT, t);
        break;
    
	case Tac::LNOT:
        emitUnaryTac(RiscvInstr::SEQZ, t);
        break;
    

    case Tac::ADD:
        emitBinaryTac(RiscvInstr::ADD, t);
        break;

	case Tac::SUB:
        emitBinaryTac(RiscvInstr::SUB, t);
        break;

	case Tac::MUL:
        emitBinaryTac(RiscvInstr::MUL, t);
        break;

	case Tac::DIV:
        emitBinaryTac(RiscvInstr::DIV, t);
        break;

	case Tac::MOD:
        emitBinaryTac(RiscvInstr::REM, t);
        break;

	case Tac::GTR:
		emitBinaryTac(RiscvInstr::SGT, t);
		break;

	case Tac::LES:
		emitBinaryTac(RiscvInstr::SLT, t);
		break;

	case Tac::GEQ:
		emitBinaryTac(RiscvInstr::SGE, t);
		break;

	case Tac::LEQ:
		emitBinaryTac(RiscvInstr::SLE, t);
		break;

	case Tac::EQU:
		emitBinaryTac(RiscvInstr::SEQ, t);
		break;

	case Tac::NEQ:
		emitBinaryTac(RiscvInstr::SNE, t);
		break;

	case Tac::LAND:
		emitBinaryTac(RiscvInstr::SAND, t);
		break;		

	case Tac::LOR:
		emitBinaryTac(RiscvInstr::SOR, t);
		break;	

	case Tac::ASSIGN:
		emitAssignTac(RiscvInstr::MOVE, t);
		break;
	
	case Tac::PUSH:
		// emitPushTac(t);
		break;

	case Tac::POP:
		emitPopTac(t);
		break;
	
	case Tac::PASS_PARAM:
		emitPassParamTac(t);
		break;

	case Tac::PROCESS_PARAM:
		emitProcessParamTac(t);
		break;

	case Tac::CALL:
		emitCallTac(t);
		break;

	case Tac::ALLOC:
		emitAllocTac(t);
		break;

	case Tac::MEMSET:
		emitMemsetTac(t);
		break;

    default:
        mind_assert(false); // should not appear inside a basic block
    }
}

/* Translates a LoadImm4 TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the LoadImm4 TAC
 */
void RiscvDesc::emitLoadImm4Tac(Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var))
        return;

    // uses "load immediate number" instruction
    int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
    addInstr(RiscvInstr::LI, _reg[r0], NULL, NULL, t->op1.ival, EMPTY_STR,
             NULL);
}

void RiscvDesc::emitLoadSymbolTac(Tac *t) {
    int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
    addInstr(RiscvInstr::LA, _reg[r0], NULL, NULL, 0, t->op1.name,
             NULL);
}

void RiscvDesc::emitLoadTac(Tac *t) {
	Set<Temp>* liveness = t->LiveOut->clone();
    liveness->add(t->op1.var);
	int r0 = getRegForWrite(t->op0.var, 0, 0, liveness);
	int r1 = getRegForRead(t->op1.var, 0, liveness);
    addInstr(RiscvInstr::LW, _reg[r0], _reg[r1], NULL, t->op1.offset, EMPTY_STR,
             NULL);
}

void RiscvDesc::emitStoreTac(Tac *t) {
	Set<Temp>* liveness = t->LiveOut->clone();
    liveness->add(t->op1.var);
	int r0 = getRegForRead(t->op0.var, 0, liveness);
	int r1 = getRegForWrite(t->op1.var, 0, 0, liveness);
    addInstr(RiscvInstr::SW, _reg[r0], _reg[r1], NULL, t->op1.offset, EMPTY_STR,
             NULL);
}

void RiscvDesc::emitAllocTac(Tac *t) {
	int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
    addInstr(RiscvInstr::ADDI, _reg[RiscvReg::SP], _reg[RiscvReg::SP], NULL, -t->op1.size, EMPTY_STR, NULL);
    addInstr(RiscvInstr::MOVE, _reg[r0], _reg[RiscvReg::SP], NULL, 0, EMPTY_STR, NULL);
}

void RiscvDesc::emitMemsetTac(Tac *t) {
	int addr = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
	int val = getRegForRead(t->op1.var, 0, t->LiveOut);
	for (int offset = 0; offset < t->op1.size; offset = offset + 4) {
		addInstr(RiscvInstr::SW, _reg[val], _reg[addr], NULL, offset, EMPTY_STR, NULL);
	}
}

/* Translates a Unary TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Unary TAC
 */
void RiscvDesc::emitUnaryTac(RiscvInstr::OpCode op, Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var))
        return;

    int r1 = getRegForRead(t->op1.var, 0, t->LiveOut);
    int r0 = getRegForWrite(t->op0.var, r1, 0, t->LiveOut);

    addInstr(op, _reg[r0], _reg[r1], NULL, 0, EMPTY_STR, NULL);
}

/* Translates a Binary TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Binary TAC
 */
void RiscvDesc::emitBinaryTac(RiscvInstr::OpCode op, Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var))
        return;

    Set<Temp>* liveness = t->LiveOut->clone();
    liveness->add(t->op1.var);
    liveness->add(t->op2.var);
    int r1 = getRegForRead(t->op1.var, 0, liveness);
    int r2 = getRegForRead(t->op2.var, r1, liveness);
    int r0 = getRegForWrite(t->op0.var, r1, r2, liveness);

    switch (op)
	{
	case RiscvInstr::SGE:
		addInstr(RiscvInstr::SLT, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, NULL);
    	addInstr(RiscvInstr::XORI, _reg[r0], _reg[r0], NULL, 1, EMPTY_STR, NULL);
		break;

	case RiscvInstr::SLE:
		addInstr(RiscvInstr::SGT, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, NULL);
    	addInstr(RiscvInstr::XORI, _reg[r0], _reg[r0], NULL, 1, EMPTY_STR, NULL);
		break;

	case RiscvInstr::SEQ:
		addInstr(RiscvInstr::SUB, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, NULL);
    	addInstr(RiscvInstr::SEQZ, _reg[r0], _reg[r0], NULL, 1, EMPTY_STR, NULL);
		break;

	case RiscvInstr::SNE:
		addInstr(RiscvInstr::SUB, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, NULL);
    	addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, NULL);
		break;

	case RiscvInstr::SOR:
		addInstr(RiscvInstr::OR, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, NULL);
    	addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, NULL);
		break;

	case RiscvInstr::SAND:
		// snez d, s1; sub d, zero, d; and d, d, s2; snez d, d;
		addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r1], NULL, 0, EMPTY_STR, NULL);
		addInstr(RiscvInstr::NEG, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, NULL);
		addInstr(RiscvInstr::AND, _reg[r0], _reg[r0], _reg[r2], 0, EMPTY_STR, NULL);
		addInstr(RiscvInstr::SNEZ, _reg[r0], _reg[r0], NULL, 0, EMPTY_STR, NULL);
		break;
	
	default:
		addInstr(op, _reg[r0], _reg[r1], _reg[r2], 0, EMPTY_STR, NULL);
		break;
	}
	
}

/* Translates an Assign TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Assign TAC
 */
void RiscvDesc::emitAssignTac(RiscvInstr::OpCode op, Tac *t) {
    // eliminates useless assignments
    if (!t->LiveOut->contains(t->op0.var))
        return;

    int r1 = getRegForRead(t->op1.var, 0, t->LiveOut);
    int r0 = getRegForWrite(t->op0.var, r1, 0, t->LiveOut);

    addInstr(op, _reg[r0], _reg[r1], NULL, 0, EMPTY_STR, NULL);
}

/* Translates a PassParam TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the PassParam TAC
 */
void RiscvDesc::emitPassParamTac(Tac *t){
	int order = t->op0.order;	// parameter order
	if (order < 8){ // pass via register
		passParamReg(t, order);
	}
	else{ // pass via stack
		int total_arg_num = t->op0.total_arg_num;
		if (order == 8){
			int new_stack_size = WORD_SIZE * (total_arg_num - 8);
			addInstr(RiscvInstr::ADDI, _reg[RiscvReg::SP], _reg[RiscvReg::SP], 
					 NULL, -new_stack_size, EMPTY_STR, NULL);
		}
		Set<Temp>* liveness = t->LiveOut->clone();
    	liveness->add(t->op0.var);
		int r0 = getRegForRead(t->op0.var, 0, liveness);
		addInstr(RiscvInstr::SW, _reg[r0],  _reg[RiscvReg::SP], NULL, 
				 WORD_SIZE * (order-8), EMPTY_STR, NULL);
	}
} 

/* Translates a ProcseeParam TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the ProcessParam TAC
 */
void RiscvDesc::emitProcessParamTac(Tac *t){
	int order = t->op0.order;	// parameter order
	if (order < 8){ // pass via register
		int r0 = getRegForRead(t->op0.var, 0, t->LiveOut);    
	 	addInstr(RiscvInstr::MOVE, _reg[r0], _reg[RiscvReg::A0 + order], NULL, 0, EMPTY_STR, NULL);
		_reg[r0]->dirty = true;
	}
	else{ // pass via stack
		int r0 = getRegForWrite(t->op0.var, RiscvReg::FP, 0, t->LiveOut);
		addInstr(RiscvInstr::LW, _reg[r0],  _reg[RiscvReg::FP], NULL, 
				WORD_SIZE * (order-8) + 44, EMPTY_STR, NULL);
	}
}

/* Translates a Pop TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Pop TAC
 */
void RiscvDesc::emitPopTac(Tac *t){
	int order = t->op0.order;	// parameter order
	if (order < 8){ // pass via register
		// move from param register
		std::ostringstream oss;
    	Temp v = t->op0.var;
    	if ((NULL != v) && t->LiveOut->contains(v)) {
        	int r0 = getRegForRead(v, 0, t->LiveOut);
			addInstr(RiscvInstr::MOVE, _reg[r0],  _reg[RiscvReg::A0 + order], NULL, 0,
					 EMPTY_STR, NULL);
			// pop old param register from stack
			addInstr(RiscvInstr::LW, _reg[RiscvReg::A0 + order], _reg[RiscvReg::SP], NULL, -order*4-4, 
					EMPTY_STR, NULL);
    	}
	}
	else{ // pass via stack
		int total_arg_num = t->op0.total_arg_num;
		int r0 = getRegForWrite(t->op0.var, RiscvReg::SP, 0, t->LiveOut);
		if (r0 != 0){
			addInstr(RiscvInstr::LW, _reg[r0],  _reg[RiscvReg::SP], NULL, 
					WORD_SIZE * (order-8), EMPTY_STR, NULL);
		}
		
		if (order + 1 == total_arg_num){
			int new_stack_size = WORD_SIZE * (total_arg_num - 8);
			addInstr(RiscvInstr::ADDI, _reg[RiscvReg::SP], _reg[RiscvReg::SP], 
					 NULL, new_stack_size, EMPTY_STR, NULL);
		}
	}} 

/* Translates a Call TAC into Riscv instructions.
 *
 * PARAMETERS:
 *   t     - the Call TAC
 */
void RiscvDesc::emitCallTac(Tac *t){
	// 1. save caller-saved registers and temp variables
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
		_old_reg[i]->dirty = false;
		_old_reg[i]->var = NULL;

		Temp v = _reg[i]->var;
		if (_reg[i]->saver == CALLER && 
			(NULL != v) && t->LiveOut->contains(v)){
			spillReg(i, t->LiveOut);
			_old_reg[i]->dirty = true;
			_old_reg[i]->var = v;
		}
    }

	// 2. function call
	addInstr(RiscvInstr::CALL, NULL, NULL, NULL, 0, std::string("_") + t->op1.label->str_form, NULL);
	
	// 4. save return value to dest register
	int r0 = getRegForWrite(t->op0.var, 0, 0, t->LiveOut);
	addInstr(RiscvInstr::MOVE, _reg[r0], _reg[RiscvReg::A0], NULL, 0, EMPTY_STR, "return value");
	_reg[RiscvReg::A0]->var = NULL;
	_reg[r0]->var = t->op0.var;
	
	// 3. recover caller-saved registers and temp variables
	t->LiveOut->add(t->op0.var);
	 for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
       if (_reg[i]->saver == CALLER && 
			(NULL != _old_reg[i]->var) && _old_reg[i]->dirty && t->LiveOut->contains(_old_reg[i]->var)){
			recoverReg(i, t->LiveOut);
		}
    }
	
} 

/* Outputs a single instruction line.
 *
 * PARAMETERS:
 *   label   - label of this line
 *   body    - instruction
 *   comment - comment of this line
 */
void RiscvDesc::emit(std::string label, const char *body, const char *comment) {
    std::ostream &os(*_result);

    if ((NULL != comment) && (label.empty()) && (NULL == body)) {
        os << "                                  # " << comment;

    } else {
        if (!label.empty())
            os << label << std::left << std::setw(40 - label.length()) << ":";
        else if (NULL != body)
            os << "          " << std::left << std::setw(30) << body;

        if (NULL != comment)
            os << "# " << comment;
    }

    os << std::endl;
}

/* Use to put a specified reg to another to pass params.
 *
 * PARAMETERS:
 *   t     - a special tac for param
 *   cnt   - reg offset A0 + cnt
 */
void RiscvDesc::passParamReg(Tac *t, int cnt) {
    t->LiveOut->add(t->op0.var);
    std::ostringstream oss;
    // RISC-V use a0-a7 to pass the first 8 parameters, so it's ok to do so.
	spillReg(RiscvReg::A0 + cnt, t->LiveOut);
    int i = lookupReg(t->op0.var);
    if(i < 0) {
        auto v = t->op0.var;
        RiscvReg *base = _reg[RiscvReg::FP];
        oss << "    load " << v << " from (" << base->name
            << (v->offset < 0 ? "" : "+") << v->offset << ") into "
            << _reg[RiscvReg::A0 + cnt]->name;
        addInstr(RiscvInstr::LW, _reg[RiscvReg::A0 + cnt], base, NULL, v->offset, EMPTY_STR,
                    oss.str().c_str());
    } else {
        oss << "    copy " << _reg[i]->name << " to " << _reg[RiscvReg::A0 + cnt]->name;
        addInstr(RiscvInstr::MOVE, _reg[RiscvReg::A0 + cnt], _reg[i], NULL, 0,
                    EMPTY_STR, oss.str().c_str());
		_reg[i]->var = NULL;
		_reg[i]->dirty = true;
    }
	_reg[RiscvReg::A0 + cnt]->var = t->op0.var;
    _reg[RiscvReg::A0 + cnt]->dirty = true;
}

/* Use to set a param reg.
 *
 * PARAMETERS:
 *   t     - a special tac for param
 *   cnt   - reg offset A0 + cnt
 */
void RiscvDesc::setParamReg(Tac *t, int cnt) {
    _reg[RiscvReg::A0 + cnt]->var = t->op0.var;
    _reg[RiscvReg::A0 + cnt]->dirty = true;
}

/* Translates a "Functy" object into assembly code and output.
 *
 * PARAMETERS:
 *   f     - the Functy object
 */
void RiscvDesc::emitFuncty(Functy f) {
    mind_assert(NULL != f);

    _frame = new RiscvStackFrameManager(-3 * WORD_SIZE);
    FlowGraph *g = FlowGraph::makeGraph(f);
    g->simplify();        // simple optimization
    g->analyzeLiveness(); // computes LiveOut set of the basic blocks

    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it) {
        // all variables shared between basic blocks should be reserved
        Set<Temp> *liveout = (*it)->LiveOut;
        for (Set<Temp>::iterator sit = liveout->begin(); sit != liveout->end();
             ++sit) {
            _frame->reserve(*sit);
        }
        (*it)->entry_label = getNewLabel(); // adds entry label of a basic block
    }
    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it) {
        BasicBlock *b = *it;
        b->analyzeLiveness(); // computes LiveOut set of every TAC
        _frame->reset();
        // translates the TAC sequences of this block
        b->instr_chain = prepareSingleChain(b, g);
        if (Option::doOptimize()) // use "-O" option to enable optimization
            simplePeephole((RiscvInstr *)b->instr_chain);
        b->mark = 0; // clears the marks (for the next step)
    }
    if (Option::getLevel() == Option::DATAFLOW) {
        std::cout << "Control-flow Graph of " << f->entry << ":" << std::endl;
        g->dump(std::cout);
        // TO STUDENTS: You might not want to get lots of outputs when
        // debugging.
        //              You can enable the following line so that the program
        //              will terminate after the first Functy is done.
        // std::exit(0);
        return;
    }

    mind_assert(!f->entry->str_form.empty()); // this assertion should hold for every Functy
    // outputs the header of a function
    emitProlog(f->entry, _frame->getStackFrameSize());
    // chains up the assembly code of every basic block and output.
    //
    // ``A trace is a sequence of statements that could be consecutively
    //   executed during the execution of the program. It can include
    //   conditional branches.''
    //           -- Modern Compiler Implementation in Java (the ``Tiger Book'')
    for (FlowGraph::iterator it = g->begin(); it != g->end(); ++it)
        emitTrace(*it, g);
}

/* Outputs the leading code of a function.
 *
 * PARAMETERS:
 *   entry_label - the function label
 *   frame_size  - stack-frame size of this function
 * NOTE:
 *   the prolog code is used to save context and establish the stack frame.
 */
void RiscvDesc::emitProlog(Label entry_label, int frame_size) {
    std::ostringstream oss;

    emit(EMPTY_STR, NULL, NULL); // an empty line
    emit(EMPTY_STR, ".text", NULL);
    if (entry_label->str_form == "main") {
        oss << "main";
    } else {
        oss << entry_label;
    }
    emit(oss.str(), NULL, "function entry"); // marks the function entry label
    oss.str("");
    // saves old context
    emit(EMPTY_STR, "sw    ra, -4(sp)", NULL); // saves old frame pointer
    emit(EMPTY_STR, "sw    fp, -8(sp)", NULL); // saves return address
    // establishes new stack frame (new context)
    emit(EMPTY_STR, "mv    fp, sp", NULL);
    // oss << "addi  sp, sp, -" << (frame_size + 2 * WORD_SIZE); // 2 WORD's for old $fp and $ra
	oss << "addi  sp, sp, -" << (frame_size + 13 * WORD_SIZE); // 2 WORD's for old $fp and $ra, 11 WORD's for old $s1 - $s11
    emit(EMPTY_STR, oss.str().c_str(), NULL);
	oss.str("");

	// save calle-saved registers
	emit(EMPTY_STR, NULL, "save callee-saved registers"); // marks the function entry label
    oss.str("");
	int cnt = -12;
	for (int i = RiscvReg::S1; i <= RiscvReg::S11; i++){
		if (_reg[i]->saver == CALLEE){
			oss << "sw  " << _reg[i]->name << ", " << cnt << "(fp)";
			emit(EMPTY_STR, oss.str().c_str(), NULL);
			oss.str("");
			cnt -= 4;
		}
	}
	emit(EMPTY_STR, "addi    fp, fp, -44", NULL);
}

/* Outputs a single instruction.
 *
 * PARAMETERS:
 *   i     - the instruction to output
 */
void RiscvDesc::emitInstr(RiscvInstr *i) {
    if (i->cancelled)
        return;

    std::ostringstream oss;
    oss << std::left << std::setw(6);

    switch (i->op_code) {
    case RiscvInstr::COMMENT:
        emit(EMPTY_STR, NULL, i->comment);
        return;

    case RiscvInstr::LI:
        oss << "li" << i->r0->name << ", " << i->i;
        break;

    case RiscvInstr::NEG:
        oss << "neg" << i->r0->name << ", " << i->r1->name;
        break;

	case RiscvInstr::NOT:
        oss << "not" << i->r0->name << ", " << i->r1->name;
        break;

	case RiscvInstr::OR:
		oss << "or" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
		break;

	case RiscvInstr::AND:
		oss << "and" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
		break;

	case RiscvInstr::XORI:
		oss << "xori" << i->r0->name << ", " << i->r1->name << ", " << i->i;
		break;

	case RiscvInstr::SEQZ:
        oss << "seqz" << i->r0->name << ", " << i->r1->name;
        break;

    case RiscvInstr::MOVE:
        oss << "mv" << i->r0->name << ", " << i->r1->name;
        break;

    case RiscvInstr::LW:
        oss << "lw" << i->r0->name << ", " << i->i << "(" << i->r1->name << ")";
        break;

    case RiscvInstr::SW:
        oss << "sw" << i->r0->name << ", " << i->i << "(" << i->r1->name << ")";
        break;

	case RiscvInstr::LA:
		 oss << "la" << i->r0->name << ", " << i->l;
        break;
    
    case RiscvInstr::RET:
        oss << "ret";
        break;
    
    case RiscvInstr::ADD:
        oss << "add" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;
    
    case RiscvInstr::ADDI:
		oss << "addi" << i->r0->name << ", " << i->r1->name << ", " << i->i;
		break;
	
	case RiscvInstr::SUB:
        oss << "sub" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

	case RiscvInstr::MUL:
        oss << "mul" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::DIV:
        oss << "div" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::REM:
        oss << "rem" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
        break;

    case RiscvInstr::BEQZ:
        oss << "beqz" << i->r0->name << ", " << i->l;
        break;

    case RiscvInstr::SNEZ:
        oss << "snez" << i->r0->name << ", " << i->r1->name;
        break;

	case RiscvInstr::SGT:
		oss << "sgt" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
		break;
	
	case RiscvInstr::SLT:
		oss << "slt" << i->r0->name << ", " << i->r1->name << ", " << i->r2->name;
		break;

    case RiscvInstr::J:
        oss << "j" << i->l;
        break;
	
	case RiscvInstr::CALL:
		oss << "call" << i->l;
		break;

    default:
        mind_assert(false); // other instructions not supported
    }

    emit(EMPTY_STR, oss.str().c_str(), i->comment);
}

/* Outputs a "trace" (see also: RiscvDesc::emitFuncty).
 *
 * PARAMETERS:
 *   b     - the leading basic block of this trace
 *   g     - the control-flow graph
 * NOTE:
 *   we just do a simple depth-first search against the CFG
 */
void RiscvDesc::emitTrace(BasicBlock *b, FlowGraph *g) {
    // a trace is a series of consecutive basic blocks
    if (b->mark > 0)
        return;
    b->mark = 1;
    emit(std::string(b->entry_label), NULL, NULL);

    RiscvInstr *i = (RiscvInstr *)b->instr_chain;
    while (NULL != i) {
        emitInstr(i);
        i = i->next;
    }
    switch (b->end_kind) {
    case BasicBlock::BY_JUMP:
        emitTrace(g->getBlock(b->next[0]), g);
        break;

    case BasicBlock::BY_JZERO:
        emitTrace(g->getBlock(b->next[1]), g);
        break;

    case BasicBlock::BY_RETURN:
        break;

    default:
        mind_assert(false); // unreachable
    }
}

/* Appends an instruction line to "_tail". (internal helper function)
 *
 * PARAMETERS:
 *   op_code - operation code
 *   r0      - the first register operand (if any)
 *   r1      - the second register operand (if any)
 *   r2      - the third register operand (if any)
 *   i       - immediate number or offset (if any)
 *   l       - label operand (for LA and jumps)
 *   cmt     - comment of this line
 */
void RiscvDesc::addInstr(RiscvInstr::OpCode op_code, RiscvReg *r0, RiscvReg *r1,
                         RiscvReg *r2, int i, std::string l, const char *cmt) {
    mind_assert(NULL != _tail);

    // we should eliminate all the comments when doing optimization
    if (Option::doOptimize() && (RiscvInstr::COMMENT == op_code))
        return;
    _tail->next = new RiscvInstr();
    _tail = _tail->next;
    _tail->op_code = op_code;
    _tail->r0 = r0;
    _tail->r1 = r1;
    _tail->r2 = r2;
    _tail->i = i;
    _tail->l = l;
    _tail->comment = cmt;
}


/******************** a simple peephole optimizer *********************/

/* Performs a peephole optimization pass to the instruction sequence.
 *
 * PARAMETERS:
 *   iseq  - the instruction sequence to optimize
 */
void RiscvDesc::simplePeephole(RiscvInstr *iseq) {
    // if you are interested in peephole optimization, you can implement here
    // of course, beyond our requirements
    
}

/******************* REGISTER ALLOCATOR ***********************/

/* Acquires a register to read some variable.
 *
 * PARAMETERS:
 *   v      - the variable to read
 *   avoid1 - the register which should not be selected
 *   live   - current liveness set
 * RETURNS:
 *   number of the register containing the content of v
 */
int RiscvDesc::getRegForRead(Temp v, int avoid1, LiveSet *live) {
    std::ostringstream oss;

	// if (v->id == 8){
	// 	if (NULL != _reg[RiscvReg::A0]->var)
	// 		std::cout << "(1) T8 getRegRead: reg[" << _reg[10]->name << "]->var = T" << _reg[5]->var->id << std::endl;
	// 	else
	// 		std::cout << "(1) T8 getRegRead: reg[" << _reg[10]->name << "]->var = NULL" << std::endl;
	// }

    int i = lookupReg(v);
	// if (v->id == 8){
	// 	std::cout << "(2) T8 getRegRead lookup = " << i << std::endl;
	// 	if (NULL != _reg[10]->var)
	// 		std::cout << "(2) T8 getRegRead: reg[" << _reg[10]->name << "]->var = T" << _reg[10]->var->id << std::endl;
	// }

    if (i < 0) {
        // we will load the content into some register
        i = lookupReg(NULL);
		// if (v->id == 8)
		// 	std::cout << "(3) T8 getRegRead lookup = " << i << std::endl;

        if (i < 0) {
            i = selectRegToSpill(avoid1, RiscvReg::ZERO, live);
			// if (v->id == 8)
			// 	std::cout << "(4) T8 getRegRead lookup = " << i << std::endl;
            spillReg(i, live);
        }

        _reg[i]->var = v;

        if (v->is_offset_fixed) {
            RiscvReg *base = _reg[RiscvReg::FP];
            oss << "load " << v << " from (" << base->name
                << (v->offset < 0 ? "" : "+") << v->offset << ") into "
                << _reg[i]->name;
            addInstr(RiscvInstr::LW, _reg[i], base, NULL, v->offset, EMPTY_STR,
                     oss.str().c_str());

        } else {
            oss << "initialize " << v << " with 0";
            addInstr(RiscvInstr::MOVE, _reg[i], _reg[RiscvReg::ZERO], NULL, 0,
                     EMPTY_STR, oss.str().c_str());
        }
        _reg[i]->dirty = false;
		// if (v->id == 8){
		// 	std::cout << "(5) T8 getRegRead: reg[" << _reg[i]->name << "]->var = T" << _reg[i]->var->id << std::endl;
		// }
    }

    return i;
}

/* Acquires a register to write some variable.
 *
 * PARAMETERS:
 *   v      - the variable to write
 *   avoid1 - the register which should not be selected
 *   avoid2 - the same as "avoid1"
 *   live   - the current liveness set
 * RETURNS:
 *   number of the register which can be safely written to
 */
int RiscvDesc::getRegForWrite(Temp v, int avoid1, int avoid2, LiveSet *live) {
    if (NULL == v || !live->contains(v))
        return RiscvReg::ZERO;

    int i = lookupReg(v); // look for reg[i]->var == v

    if (i < 0) {
        i = lookupReg(NULL); // look for empty reg

        if (i < 0) { // no empty reg available
            i = selectRegToSpill(avoid1, avoid2, live);
            spillReg(i, live);
        }
        _reg[i]->var = v;
    }

    _reg[i]->dirty = true;

    return i;
}

/* Spills a specified register (into memory, i.e. into the stack-frame).
 *
 * PARAMETERS:
 *   i     - number of the register to spill
 *   live  - the current liveness set
 * NOTE:
 *   if the variable contained in $i is no longer alive,
 *   we don't save it into memory.
 */
void RiscvDesc::spillReg(int i, LiveSet *live) {
    std::ostringstream oss;

    Temp v = _reg[i]->var;

    if ((NULL != v) && _reg[i]->dirty && live->contains(v)) {
        RiscvReg *base = _reg[RiscvReg::FP];

        if (!v->is_offset_fixed) {
            _frame->getSlotToWrite(v, live);
        }

        oss << "spill " << v << " from " << _reg[i]->name << " to ("
            << base->name << (v->offset < 0 ? "" : "+") << v->offset << ")";
        addInstr(RiscvInstr::SW, _reg[i], base, NULL, v->offset, EMPTY_STR,
                 oss.str().c_str());
    }

    _reg[i]->var = NULL;
    _reg[i]->dirty = false;
}

/* Recovers a specified register (from memory, i.e. into the stack-frame).
 *
 * PARAMETERS:
 *   i     - number of the register to recover
 *   live  - the current liveness set
 * NOTE:
 *   if the variable contained in $i is no longer alive,
 *   we don't recover it from memory.
 */
void RiscvDesc::recoverReg(int i, LiveSet *live) {
    std::ostringstream oss;

    Temp v = _old_reg[i]->var;

    if ((NULL != v) && _old_reg[i]->dirty && live->contains(v)) {
        RiscvReg *base = _reg[RiscvReg::FP];

        mind_assert(v->is_offset_fixed);

        oss << "recover " << v << " from (" << base->name << (v->offset < 0 ? "" : "+") << v->offset << ")"
			<< " to " << _reg[i]->name ;
        spillReg(i, live);
		addInstr(RiscvInstr::LW, _reg[i], base, NULL, v->offset, EMPTY_STR,
                 oss.str().c_str());

		_reg[i]->var = v;
    	_reg[i]->dirty = true;
		_old_reg[i]->var = NULL;
		_old_reg[i]->dirty = false;
    }


}

/* Spills all dirty (and alive) registers into memory.
 *
 * PARAMETERS:
 *   live  - the current liveness set
 */
void RiscvDesc::spillDirtyRegs(LiveSet *live) {
    int i;
    // determines whether we should spill the registers
    for (i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if ((NULL != _reg[i]->var) && _reg[i]->dirty &&
            live->contains(_reg[i]->var))
            break;

        _reg[i]->var = NULL;
        _reg[i]->dirty = false;
    }

    if (i < RiscvReg::TOTAL_NUM) {
        addInstr(RiscvInstr::COMMENT, NULL, NULL, NULL, 0, EMPTY_STR,
                 "(save modified registers before control flow changes)");

        for (; i < RiscvReg::TOTAL_NUM; ++i)
            spillReg(i, live);
    }
}

/* Looks up a register containing the specified variable.
 *
 * PARAMETERS:
 *   v     - the specified variable
 * RETURNS:
 *   number of the register if found; -1 if not found
 */
int RiscvDesc::lookupReg(tac::Temp v) {
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i)
        if (_reg[i]->general && _reg[i]->var == v)
            return i;

    return -1;
}

/* Selects a register to spill into memory (so that it can be released).
 *
 * PARAMETERS:
 *   avoid1 - the register that should not be selected
 *   avoid2 - the same as avoid1
 *   live   - the current liveness set
 * RETURNS:
 *   number of the selected register
 */
int RiscvDesc::selectRegToSpill(int avoid1, int avoid2, LiveSet *live) {
    // looks for a "ready" one
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if (!_reg[i]->general)
            continue;

        if ((i != avoid1) && (i != avoid2) && !live->contains(_reg[i]->var))
            return i;
    }

    // looks for a clean one (so that we could save a "store")
    for (int i = 0; i < RiscvReg::TOTAL_NUM; ++i) {
        if (!_reg[i]->general)
            continue;

        if ((i != avoid1) && (i != avoid2) && !_reg[i]->dirty)
            return i;
    }

    // the worst case: all are live and all are dirty.
    // chooses one register w.r.t a policy similar to the LRU algorithm (random
    // choice)
    do {
        _lastUsedReg = (_lastUsedReg + 1) % RiscvReg::TOTAL_NUM;
    } while ((_lastUsedReg == avoid1) || (_lastUsedReg == avoid2) ||
             !_reg[_lastUsedReg]->general);

    return _lastUsedReg;
}
