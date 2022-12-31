/*****************************************************
 *  AST Translation Helper.
 *
 *  NOTE: You should use this class to generate Tac's.
 *        Please don't use Tac::XXX directly.
 *
 */

#ifndef __MIND_TRANSHELPER__
#define __MIND_TRANSHELPER__

#include "define.hpp"
#include "tac/tac.hpp"

namespace mind {

#define MIND_TRANSHELPER_DEFINED
namespace tac {

/** Translation helper.
 *
 *  We use a helper to generate Tac's instead of calling the
 *  creationg functions in Tac.
 */
class TransHelper {
  public:
    // constructor
    TransHelper(assembly::MachineDesc *);
    // gets the offset counter of the target machine
    assembly::OffsetCounter *getOffsetCounter(void);
    // allocates a new int32 temporary variable
    Temp getNewTempI4(void);
    // allocates a new label
    Label getNewLabel(void);
    // allocates a new entry Label object for function
    Label getNewEntryLabel(symb::Function *);
	// allocates space on stack for array
	Temp genAlloc(int);
	void genMemset(Temp start_addr, Temp value, int length);
    // starts to translate a function
    void startFunc(symb::Function *);
    // ends translating a function
    void endFunc(void);
	// generates a GlobalVar object.
	void genGlobalVarible(std::string name, int value, int size);
	void genGlobalVarible(std::string name, ast::DimList *value, int size);

    // Arithmetic
    Temp genAdd(Temp, Temp);
    Temp genSub(Temp, Temp);
    Temp genMul(Temp, Temp);
    Temp genDiv(Temp, Temp);
    Temp genMod(Temp, Temp);
    Temp genEqu(Temp, Temp);
    Temp genNeq(Temp, Temp);
    Temp genLes(Temp, Temp);
    Temp genLeq(Temp, Temp);
    Temp genGtr(Temp, Temp);
    Temp genGeq(Temp, Temp);
    Temp genNeg(Temp);
    // Logical
    Temp genLAnd(Temp, Temp);
    Temp genLOr(Temp, Temp);
    Temp genLNot(Temp);
    // Bitwise
    Temp genBNot(Temp);
    // Memory Access
    void genPop(Temp, int);
	void genClearArgs(void);
	void genPushToStack(Temp);
	// Function
	void genPassParam(Temp, int, int);
	void genProcessParam(Temp, int);
	void genPushToReg(Temp);
	Temp genCall(Label label, int);
    // Control-flow related
    void genJump(Label);
    void genJumpOnZero(Label, Temp);
    void genReturn(Temp);
    // Miscellaneous
    void genAssign(Temp, Temp);
    Temp genLoadImm4(int);
    void genMarkLabel(Label);
    void genMemo(const char *);
	// Global Variable
	Temp genLoadSym(std::string);
	Temp genLoad(Temp addr, int offset);
	void genStore(Temp src, Temp addr, int offset);

    // gets the entire Piece list
    Piece *getPiece();

  private:
    // the machine description
    assembly::MachineDesc *mach;
    // the Piece list
    Piece head, *ptail;
    // the Tac list of a function
    Tac *tacs, *tacs_tail;
    // the current Function
    symb::Function *current;
    // counters for temporaries and labels (for their index)
    int var_count, label_count;
    // whether the startup code has been generated
    bool startup_ok;

    // appends a Tac to the Tac list
    void chainUp(Tac *);
    // gets a Memo about the Function object
    Tac *memoOf(symb::Function *);
};
} // namespace tac
} // namespace mind

#endif // __MIND_TRANSHELPER__
