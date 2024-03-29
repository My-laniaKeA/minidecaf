/*****************************************************
*  The GNU Bison Specification for Mind Language.
*
*  We have provided complete SECTION I & IV for you.
*  Please complete SECTION II & III.
*
*  In case you want some debug support, we provide a
*  "diagnose()" function for you. All you need is to
*  call this function in main.cpp.
*
*  Please refer to the ``GNU Flex Manual'' if you have
*  problems about how to write the lexical rules.
*
*  Keltin Leung 
*/
%output "parser.cpp"
%skeleton "lalr1.cc"
%defines
%define api.value.type variant
%define api.token.constructor
%define parse.assert
%locations
/* SECTION I: preamble inclusion */
%code requires{
#include "config.hpp"
#include "ast/ast.hpp"
#include "location.hpp"
#include "parser.hpp"

using namespace mind;

void yyerror (char const *);
void setParseTree(ast::Program* tree);

/* This macro is provided for your convenience. */
#define POS(pos)    (new Location(pos.begin.line, pos.begin.column))


void scan_begin(const char* filename);
void scan_end();
}
%code{
#include "compiler.hpp"
}
/* SECTION II: definition & declaration */

/*   SUBSECTION 2.1: token declaration */
/*   2.1.1: terminal declaration */
%define api.token.prefix {TOK_}
%token
END  0  "end of file"
BOOL "bool"
INT  "int"
RETURN "return"
IF "if"
ELSE  "else"
DO "do"
WHILE "while"
FOR "for"
BREAK "break"
CONTINUE "continue"
EQU "=="
NEQ "!="
AND "&&" 
OR "||"
LEQ "<="
GEQ ">="
PLUS "+"
MINUS "-"
TIMES "*"
SLASH "/"
MOD "%"
LT "<"
GT ">"
COLON ":"
SEMICOLON ";"
LNOT "!"
BNOT "~"
COMMA ","
DOT "."
ASSIGN "="
QUESTION "?"
LPAREN "("
RPAREN ")"
LBRACK "["
RBRACK "]"
LBRACE "{"
RBRACE "}"
;
%token <std::string> IDENTIFIER "identifier"
%token<int> ICONST "iconst"
/*   2.1.2: non-terminal declaration */
%nterm<mind::ast::StmtList*> BlockItemList
%nterm<mind::ast::VarList* > FormalList ParamList
%nterm<mind::ast::ExprList* > ExprList IndexList
%nterm<mind::ast::Program* > Program FoDList
%nterm<mind::ast::FuncDefn* > FuncDefn
%nterm<mind::ast::Type*> Type
%nterm<mind::ast::Statement*>  BlockItem Stmt ReturnStmt ExprStmt IfStmt CompStmt WhileStmt DeclrStmt ForStmt DoWhileStmt
%nterm<mind::ast::Expr*> Expr AssignExpr Conditional ForExpr
%nterm<mind::ast::Expr*> Additive Multiplicative Unary Primary Postfix
%nterm<mind::ast::Expr*> Logical_And Logical_Or 
%nterm<mind::ast::Expr*> Equality Relational
%nterm<mind::ast::DimList* > DimList IntList
/*   SUBSECTION 2.2: associativeness & precedences */
/* Lowest priority */
%nonassoc QUESTION
%left     OR
%left     AND
%left EQU NEQ
%left LEQ GEQ LT GT
%left     PLUS MINUS
%left     TIMES SLASH MOD
%nonassoc LNOT NEG BNOT
%nonassoc LBRACK DOT	
/* Highest priority */
%{
/* we have to include scanner.hpp here... */
#define YY_NO_UNISTD_H 1
%}

/*   SUBSECTION 2.5: start symbol of the grammar */
%start Program

/* SECTION III: grammar rules (and actions) */
%%
Program     : FoDList
				{ /* we don't write $$ = XXX here. */
				setParseTree($1); }
			;
FoDList :   FuncDefn 
				{ $$ = new ast::Program($1,POS(@1)); } |
			DeclrStmt
				{ $$ = new ast::Program($1,POS(@1)); } |
			FoDList FuncDefn{
				{$1->func_and_globals->append($2);
				$$ = $1; }
				} |
			FoDList DeclrStmt{
				{$1->func_and_globals->append($2);
				$$ = $1; }
				}

FuncDefn : Type IDENTIFIER LPAREN FormalList RPAREN LBRACE BlockItemList RBRACE {
			$$ = new ast::FuncDefn($2,$1,$4,$7,POS(@1));
		} |
		Type IDENTIFIER LPAREN FormalList RPAREN SEMICOLON{
			$$ = new ast::FuncDefn($2,$1,$4,new ast::EmptyStmt(POS(@6)),POS(@1));
		};
FormalList :  /* EMPTY */
				{ $$ = new ast::VarList(); } 
			| ParamList
				{ $$ = $1; }
			;
ParamList	: Type IDENTIFIER
				{ $$ = new ast::VarList(); 
				  $$->append( new ast::VarDecl($2, $1, POS(@2)) ); 
				}
			| Type IDENTIFIER DimList
				{ $$ = new ast::VarList(); 
				  $$->append( new ast::VarDecl($2, $1, $3, POS(@2)) ); 
				}
			| Type IDENTIFIER LBRACK RBRACK 
				{ $$ = new ast::VarList(); 
				  $$->append( new ast::VarDecl($2, $1, new ast::DimList(), POS(@2)) ); 
				}
			| ParamList COMMA Type IDENTIFIER
				{ $1->append( new ast::VarDecl($4, $3, POS(@4)) );
				  $$ = $1;
				}
			| ParamList COMMA Type IDENTIFIER DimList
				{ $1->append( new ast::VarDecl($4, $3, $5, POS(@4)) );
				  $$ = $1;
				}
			| ParamList COMMA Type IDENTIFIER LBRACK RBRACK 
				{ $1->append( new ast::VarDecl($4, $3, new ast::DimList(), POS(@4)) );
				  $$ = $1;
				}
			;
Type        : INT
				{ $$ = new ast::IntType(POS(@1)); }

BlockItemList    : /* empty */
				{ $$ = new ast::StmtList(); }
			| BlockItemList BlockItem
				{ $1->append($2);
				$$ = $1; }
			;

BlockItem	: Stmt 		{$$ = $1;}
			| DeclrStmt	{$$ = $1;} 
			;

Stmt        : ReturnStmt {$$ = $1;}|
			ExprStmt   {$$ = $1;}|
			IfStmt     {$$ = $1;}|
			ForStmt  	 {$$ = $1;}|
			DoWhileStmt {$$ = $1;}|
			WhileStmt  {$$ = $1;}|
			CompStmt   {$$ = $1;}|
			BREAK SEMICOLON  
				{$$ = new ast::BreakStmt(POS(@1));} |
			CONTINUE SEMICOLON 
				{ $$ = new ast::ContStmt(POS(@1));}	|
			SEMICOLON
				{$$ = new ast::EmptyStmt(POS(@1));}
			;
DeclrStmt	: Type IDENTIFIER SEMICOLON
				{$$ = new ast::VarDecl($2, $1, POS(@2));}
			| Type IDENTIFIER ASSIGN Expr SEMICOLON
				{$$ = new ast::VarDecl($2, $1, $4, POS(@2));}
			| Type IDENTIFIER DimList SEMICOLON
                { $$ = new ast::VarDecl($2, $1, $3, POS(@2)); }
			| Type IDENTIFIER DimList ASSIGN LBRACE IntList RBRACE SEMICOLON
                { $$ = new ast::VarDecl($2, $1, $3, $6, POS(@2)); }
            ;
DimList   : LBRACK ICONST RBRACK DimList
                { $$ = $4;
                  $$->append($2);
                }
            | LBRACK ICONST RBRACK
                { $$ = new ast::DimList();
                  $$->append($2);
                }
			;
IntList   	: ICONST
				{ $$ = new ast::DimList();
				  $$->append($1); }
			| IntList COMMA ICONST
				{ $1->append($3);
				  $$ = $1; }
CompStmt    : LBRACE BlockItemList RBRACE
				{$$ = new ast::CompStmt($2,POS(@1));}
			;
ForStmt		: FOR LPAREN ForExpr SEMICOLON ForExpr SEMICOLON ForExpr RPAREN Stmt
				{ $$ = new ast::ForStmt($3, $5, $7, $9, POS(@1)); }
			| FOR LPAREN DeclrStmt ForExpr SEMICOLON ForExpr RPAREN Stmt
				{ $$ = new ast::ForStmt($3, $4, $6, $8, POS(@1)); }
			;
ForExpr     : /* empty */
				{ $$ = NULL; }
			| Expr
				{ $$ = $1; }   
			;
WhileStmt   : WHILE LPAREN Expr RPAREN Stmt
				{ $$ = new ast::WhileStmt($3, $5, POS(@1)); }
			;
DoWhileStmt	: DO Stmt WHILE LPAREN Expr RPAREN SEMICOLON
				{ $$ = new ast::DoWhileStmt($2, $5, POS(@1)); }
			;
IfStmt      : IF LPAREN Expr RPAREN Stmt
				{ $$ = new ast::IfStmt($3, $5, new ast::EmptyStmt(POS(@5)), POS(@1)); }
			| IF LPAREN Expr RPAREN Stmt ELSE Stmt
				{ $$ = new ast::IfStmt($3, $5, $7, POS(@1)); }
			;

ReturnStmt  : RETURN Expr SEMICOLON
				{ $$ = new ast::ReturnStmt($2, POS(@1)); }
			;
ExprStmt    : Expr SEMICOLON
				{ $$ = new ast::ExprStmt($1, POS(@1)); } 
			;      
ExprList	: Expr
				{ $$ = new ast::ExprList();
				  $$->append($1); }
			| ExprList COMMA Expr
				{ $1->append($3);
				  $$ = $1; }
			;
Expr		: AssignExpr   
				{ $$ = $1; }
AssignExpr	: Conditional
				{ $$ = $1; }
			| IDENTIFIER ASSIGN Expr
				{ $$ = new ast::AssignExpr($1, $3, POS(@1)); }
			| IDENTIFIER IndexList ASSIGN Expr
				{ $$ = new ast::AssignExpr($1, $2, $4, POS(@1)); }
			;
Conditional	: Logical_Or	
				{ $$ = $1; }
			| Logical_Or QUESTION Expr COLON Conditional
				{ $$ = new ast::IfExpr($1, $3, $5, POS(@2)); }
			;
Logical_Or	: Logical_And	{ $$ = $1; }
			| Logical_Or OR Logical_And
				{ $$ = new ast::OrExpr($1, $3, POS(@2)); }
			;
Logical_And	: Equality
				{ $$ = $1; }
			| Logical_And AND Equality
				{ $$ = new ast::AndExpr($1, $3, POS(@2)); }
			;
Equality	: Relational
				{ $$ = $1; }
			| Equality EQU Relational
				{ $$ = new ast::EquExpr($1, $3, POS(@2)); }
			| Equality NEQ Relational
				{ $$ = new ast::NeqExpr($1, $3, POS(@2)); }
			;
Relational	: Additive		
				{ $$ = $1; }
			| Relational GT Additive
				{ $$ = new ast::GrtExpr($1, $3, POS(@2)); }
			| Relational LT Additive	
				{ $$ = new ast::LesExpr($1, $3, POS(@2)); }		
			| Relational GEQ Additive
				{ $$ = new ast::GeqExpr($1, $3, POS(@2)); }
			| Relational LEQ Additive	
				{ $$ = new ast::LeqExpr($1, $3, POS(@2)); }
			;
Additive	: Multiplicative
				{ $$ = $1; }
			| Additive PLUS Multiplicative
				{ $$ = new ast::AddExpr($1, $3, POS(@2)); }
			| Additive MINUS Multiplicative
				{ $$ = new ast::SubExpr($1, $3, POS(@2)); }
			;
Multiplicative	: Unary
				{ $$ = $1; }
			| 	Multiplicative TIMES Unary
				{ $$ = new ast::MulExpr($1, $3, POS(@2)); }
			| 	Multiplicative SLASH Unary
				{ $$ = new ast::DivExpr($1, $3, POS(@2)); }
			| 	Multiplicative MOD Unary
				{ $$ = new ast::ModExpr($1, $3, POS(@2)); }
			;
Unary    	: Postfix
				{ $$ = $1; }
			| MINUS Unary  %prec NEG
				{ $$ = new ast::NegExpr($2, POS(@1)); }
			| BNOT Unary
				{ $$ = new ast::BitNotExpr($2, POS(@1)); }
			| LNOT Unary
				{ $$ = new ast::NotExpr($2, POS(@1)); }
			;
Postfix    	: Primary
				{ $$ = $1; }
			| IDENTIFIER LPAREN ExprList RPAREN
				{ $$ = new ast::CallExpr($1, $3, POS(@1)); }
			| IDENTIFIER LPAREN RPAREN
				{ $$ = new ast::CallExpr($1, new ast::ExprList(), POS(@1)); }
			| IDENTIFIER IndexList
				{ $$ = new ast::LvalueExpr($1, $2, POS(@1)); }
			;
Primary		: ICONST
				{ $$ = new ast::IntConst($1, POS(@1)); }  
			| LPAREN Expr RPAREN
				{ $$ = $2; }
			| IDENTIFIER
				{ $$ = new ast::LvalueExpr($1, POS(@1)); }
			;
IndexList   : LBRACK Expr RBRACK IndexList
                { $4->append($2);
				  $$ = $4;
                }
            | LBRACK Expr RBRACK
                { $$ = new ast::ExprList();
                  $$->append($2);
                }
			;

%%

/* SECTION IV: customized section */
#include "compiler.hpp"
#include <cstdio>

static ast::Program* ptree = NULL;
extern int myline, mycol;   // defined in scanner.l

// bison will generate code to invoke me
void
yyerror (char const *msg) {
	err::issue(new Location(myline, mycol), new err::SyntaxError(msg));
	scan_end();
	std::exit(1);
}

// call me when the Program symbol is reduced
void
setParseTree(ast::Program* tree) {
	ptree = tree;
}

/* Parses a given mind source file.
*
* PARAMETERS:
*   filename - name of the source file
* RETURNS:
*   the parse tree (in the form of abstract syntax tree)
* NOTE:
*   should any syntax error occur, this function would not return.
*/
ast::Program*
mind::MindCompiler::parseFile(const char* filename) {  
	scan_begin(filename);
	/* if (NULL == filename)
		yyin = stdin;
	else
		yyin = std::fopen(filename, "r"); */
	yy::parser parse;
	parse();
	scan_end();
	/* if (yyin != stdin)
		std::fclose(yyin); */

	return ptree;
	}

void
yy::parser::error (const location_type& l, const std::string& m)
{
	//std::cerr << l << ": " << m << '\n';
	err::issue(new Location(l.begin.line, l.begin.column), new err::SyntaxError(m));

	scan_end();
	std::exit(1);
}
