    /* cs152-miniL phase2 */
%{

#include <stdio.h>


extern FILE * yyin;
extern int currLine;
extern int currCol;
extern int yylex(void);
void yyerror(const char *msg);
char* expected = "";
char* e_list[100][100];

int COUNT = 0;
yerrork;
%}

%union{
	char* str_val;
}




%error-verbose
%start Program
%token <str_val> IDENT;
%token <str_val> NUMBER;
%token FUNCTION;
%token BEGIN_PARAMS;
%token END_PARAMS;
%token BEGIN_LOCALS;
%token END_LOCALS;
%token BEGIN_BODY;
%token END_BODY;
%token INTEGER;
%token ARRAY;
%token ENUM;
%token OF;
%token IF;
%token THEN;
%token ENDIF;
%token ELSE;
%token WHILE;
%token DO;
%token BEGINLOOP;
%token ENDLOOP;
%token CONTINUE;
%token READ;
%token WRITE;
%token AND;
%token OR;
%token NOT;
%token TRUE;
%token FALSE;
%token RETURN;
%left SUB;
%left ADD;
%left MULT;
%left DIV;
%left MOD;
%token EQ;
%token NEQ;
%token LT;
%token GT;
%token LTE;
%token GTE;
%token SEMICOLON;
%token COLON
%token COMMA;
%token L_PAREN;
%token R_PAREN;
%token L_SQUARE_BRACKET;
%token R_SQUARE_BRACKET;
%token ASSIGN;


%% 

Program:
   Functions {
       printf("Program -> Functions \n"); 
        
   }
   | {printf("Program -> Epsilon \n");}
   | error Program {expected = "Program";}

   ;

Functions:
    Function Functions {printf("Functions -> Function Functions \n");}
    |  {printf("Functions -> Epsilon \n");}
    | error Functions {expected = "Functions";}
    ;

Declaration_Semi:
    Declaration SEMICOLON {printf("Declaration_Semi -> Declaration SEMICOLON \n");}
    | error Declaration_Semi {expected = "Declaration_semi";}
    ;

Declarations_Semi:
    Declaration_Semi Declarations_Semi {printf("Declarations_Semi -> Declaration_Semi Declarations_Semi\n");}
    | {printf("Declarations_Semi -> Epsilon \n");}
    | error Declarations_Semi {expected = "Declarations_Semi";}
    ;

Statement_Semi:
    Statement SEMICOLON {printf("Statement_Semi -> Statement SEMICOLON \n");}
    | error Statement_Semi {expected = "Statement_Semi";}
    ;

Statments_Semi:
    Statement_Semi Statments_Semi {printf("Statments_Semi -> Statement_Semi Statement_Semi\n");}
    |  {printf("Statments_Semi -> Epsilon \n");}
    | error Statments_Semi {expected = "Statements_Semi";}
    ;

Function:
    FUNCTION Ident SEMICOLON BEGIN_PARAMS Declarations_Semi END_PARAMS
    BEGIN_LOCALS Declarations_Semi END_LOCALS BEGIN_BODY Statments_Semi END_BODY 
    | error Function {expected = "Function";};

Identifiers:
    Ident COMMA Identifiers {printf("Identifiers -> Ident COMMA Identifiers \n");}
    | Ident {printf("Identifiers -> Ident \n");}
    |  {printf("Identifiers -> Epsilon \n");}
    | error Identifiers {expected = "Identifiers";}
    ;

Declaration:
    Identifiers COLON ENUM L_PAREN Identifiers R_PAREN {printf("Declaration -> Identifiers COLON ENUM L_PAREN Identifiers R_PAREN\n");}
    | Identifiers COLON INTEGER {printf("Declaration -> Identifiers COLON INTEGER\n");}
    | Identifiers COLON ARRAY L_SQUARE_BRACKET Number R_SQUARE_BRACKET OF INTEGER {printf("Declaration -> Identifiers ARRAY L_SQUARE_BRACKET Number R_SQUARE_BRACKET OF INTEGER\n");}
    | error ';' {expected = "Declaration";}
    ;

Vars:
    Var Vars {printf("Vars -> Var Vars \n");}
    | COMMA Var Vars {printf("Vars -> COMMA Var Vars\n");}
    |  {printf("Vars -> Epsilon \n");}
    | error Vars {expected = "Vars";}
    ;

Statement:
    Var ASSIGN Expression {printf("Statement -> Var ASSIGN Expression\n");}
    | IF Bool-Expr THEN Statments_Semi ENDIF {printf("Statement -> IF Bool-Expr THEN Statments_Semi ENDIF\n");}
    | IF Bool-Expr THEN Statments_Semi ELSE Statments_Semi ENDIF {printf("Statement -> WHILE Bool-Expr BEGINLOOP Statments_Semi ENDLOOP\n");}
    | WHILE Bool-Expr BEGINLOOP Statments_Semi ENDLOOP {printf("Statement -> \n");}
    | DO BEGINLOOP Statments_Semi ENDLOOP WHILE Bool-Expr {printf("Statement -> DO BEGINLOOP Statments_Semi ENDLOOP WHILE Bool-Expr\n");}
    | READ Vars  {printf("Statement -> READ Vars\n");}
    | WRITE Vars {printf("Statement -> WRITE Vars\n");}
    | CONTINUE {printf("Statement -> CONTINUE\n");}
    | RETURN Expression {printf("Statement -> RETURN Expression\n");}
    | error Statement {expected = "Statement";}
    ; 

Relation-And-Exprs:
    OR Relation-And-Expr Relation-And-Exprs {printf("Relation-And-Exprs -> OR Relation-And-Expr Relation-And-Exprs \n");}
    | {printf("Relation-And-Exprs -> Epsilon \n");}
    | error Relation-And-Exprs {expected = "Relation-And-Exprs";}
    ;

Bool-Expr:
    Relation-And-Expr Relation-And-Exprs {printf("Bool-Expr ->  Relation-And-Expr Relation-And-Exprs\n");}
    | error Bool-Expr {expected = "Bool-Expr";}
    ;

Relation-Exprs:
    AND Relation-Expr Relation-Exprs {printf("Relation-Exprs -> AND Relation-Expr Relation-Exprs\n");}
    | {printf("Relation-Exprs -> Epsilon \n");}
    | error Relation-Exprs {expected = "Relation-Exprs";}
    ;

Relation-And-Expr:
    Relation-Expr Relation-Exprs {printf("Relation-And-Expr ->  Relation-Expr Relation-Exprs\n");}
    | error Relation-And-Expr {expected = "Relation-And-Expr";}
    ;

Relation-Expr:
    NOT Expression Comp Expression {printf("Relation-Expr -> NOT Expression Comp Expression\n");}
    | NOT TRUE {printf("Relation-Expr -> NOT TRUE\n");} 
    | NOT FALSE {printf("Relation-Expr -> NOT FALSE\n");}
    | NOT L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr -> NOT L_PAREN Bool-Expr R_PAREN\n");}
    | Expression Comp Expression {printf("Relation-Expr ->  Expression Comp Expression\n");}
    | TRUE {printf("Relation-Expr -> TRUE\n");}
    | FALSE {printf("Relation-Expr -> FALSE\n");}
    | L_PAREN Bool-Expr R_PAREN {printf("Relation-Expr -> L_PAREN Bool-Expr R_PAREN\n");}
    | error Relation-Expr {expected = "Relation-Expr";}
    ;

Comp:
    EQ {printf("Comp -> EQ\n");}
    | NEQ {printf("Comp -> NEQ\n");}
    | LT {printf("Comp -> LT\n");}
    | GT {printf("Comp -> GT\n");}
    | LTE {printf("Comp -> LTE\n");}
    | GTE {printf("Comp -> GTE\n");}
    | error Comp {expected = "Comp";}
    ;

Multiplicative-Exprs:
    ADD Multiplicative-Expr Multiplicative-Exprs {printf("Multiplicative-Exprs -> ADD Multiplicative-Expr Multiplicative-Exprs\n");}
    | SUB Multiplicative-Expr Multiplicative-Exprs {printf("Multiplicative-Exprs -> SUB Multiplicative-Expr Multiplicative-Exprs\n");}
    |  {printf("Multiplicative-Exprs -> Epsilon \n");}
    | error Multiplicative-Exprs {expected = "Multiplicative-Exprs";}
    ; 

Expression:
    Multiplicative-Expr Multiplicative-Exprs {printf("Expression -> Multiplicative-Expr Multiplicative-Exprs \n");}
    | error Expression {expected = "Expression";}
    ;

Terms:
    MULT Term Terms {printf("Terms -> MULT Term Terms \n");}
    | DIV Term Terms {printf("Terms -> DIV Term Terms \n");}
    | MOD Term Terms {printf("Terms -> MOD Term Terms \n");}
    |  {printf("Terms -> Epsilon \n");}
    | error Terms {expected = "Terms";}
    ;


Multiplicative-Expr:
    Term Terms {printf("Multiplicative-Expr -> Term Terms \n");}
    | error Multiplicative-Expr {expected = "Multiplicative-Expr";}
    ;

Expressions:
    Expression Expressions {printf("Expressions -> Expression Expressions\n");}
    | COMMA Expression Expressions {printf("Expressions -> COMMA Expression Expressions\n");}
    |  {printf("Expressions -> Epsilon \n");}
    | error Expressions {expected = "Expressions";}
    ;

Term:
    SUB Var {printf("Term -> SUB Var \n");}
    | SUB Number {printf("Term -> SUB Number \n");}
    | SUB L_PAREN Expression R_PAREN {printf("Term -> SUB L_PAREN Expression R_PAREN \n");}
    | Var {printf("Term -> Var \n");}
    | Number {printf("Term -> Number \n");}
    | L_PAREN Expression R_PAREN {printf("Term -> L_PAREN Expression R_PAREN\n");}
    | Ident L_PAREN Expressions R_PAREN {printf("Term -> Ident L_PAREN Expressions R_PAREN\n");}
    | error Term {expected = "Term";}
    ;

Var:
    Ident {printf("Var -> Ident \n");}
    | Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET {printf("Var  ->  Ident L_SQUARE_BRACKET Expression R_SQUARE_BRACKET\n");}
    | error Var {expected = "Var";}
    ;


Ident:
    IDENT {printf("Ident-> IDENT %s \n",$1);}
    ;

Number:
    NUMBER {printf("Number -> NUMBER %s \n", $1);}
    ;
%% 


void yyerror(const char *msg) {
	char e_string[100];
    sprintf(e_string, "!!!Error: Line: %d, Col: %d, Symbol: %s, Expected: ", currLine, currCol, yychar);
    strcpy(e_list[COUNT], e_string);
    if(COUNT > 0){
        char temp[100];
        sprintf(temp, "%s %s !!!\n", e_list[COUNT-1], expected) ; //append last expected rule to last error
        strcpy(e_list[COUNT-1], temp);
    }
    COUNT += 1;
}

int main(int argc, char ** argv) {
	if (argc >= 2) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			yyin = stdin;
		}
	}
	else {
		yyin = stdin;
	}
	yyparse();

    //Print errors if they exist.
    if(COUNT){
        char temp[100];
        sprintf(temp, "%s %s !!!\n", e_list[COUNT], expected); //append expected rule to last error
        strcpy(e_list[COUNT], temp);
        int i;
        for(i = 0; i <= COUNT; i++){
            printf(e_list[i]);
        }
    }
	return 1;
}