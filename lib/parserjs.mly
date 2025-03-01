/* keywords */
%token <int> INT
%token <string> ID
%token CLASS PROGRAM VOID
%token AND OR NOT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token MOVE PICKBEEPER PUTBEEPER RETURN TURNLEFT TURNOFF
%token SEMICOLON
%token EOF
%token ELSE IF WHILE ITERATE
%token FRONT_IS_CLEAR FRONT_IS_BLOCKED LEFT_IS_CLEAR LEFT_IS_BLOCKED RIGHT_IS_CLEAR RIGHT_IS_BLOCKED
%token FACING_EAST NOT_FACING_EAST FACING_NORTH NOT_FACING_NORTH FACING_SOUTH NOT_FACING_SOUTH FACING_WEST NOT_FACING_WEST
%token ANY_BEEPERS_IN_BEEPER_BAG NOT_ANY_BEEPERS_IN_BEEPER_BAG
%token NEXT_TO_A_BEEPER NOT_NEXT_TO_A_BEEPER
%token SUCC PRED IS_ZERO

/* Assoc */
%left AND OR        /* lowest precedence */
%nonassoc NOT       /* highest precedence */
%right ELSE         (* Declare ELSE as right-associative *)

%start program             /* the entry point */
%type <Ast.program> program
%%
program:
    | CLASS; PROGRAM; LBRACE; defs = def*; p = program_def; RBRACE; EOF  { Ast.Program (defs, p) }
;

program_def:
    | PROGRAM; LPAREN; RPAREN; b = block            { Ast.Main b }
;

def:
    | VOID; i = ID; LPAREN; RPAREN; b = block               { (i, None, b) }
    | VOID; i = ID; LPAREN; arg = ID; RPAREN; b = block;    { (i, Some arg, b) }

block:
    | LBRACE; s = statement+; RBRACE                        { s }

statement:
    | block                                             { Ast.Block $1 }
    | statement_fun; LPAREN; RPAREN; SEMICOLON          { $1 }
    | if_statement                                      { $1 }
    | while_statement                                   { $1 }
    | iterate_statement                                 { $1 }
    | call; SEMICOLON                                   { $1 }

statement_fun:
    | MOVE                                              { Ast.Move }
    | PICKBEEPER                                        { Ast.PickBeeper }
    | PUTBEEPER                                         { Ast.PutBeeper }
    | RETURN                                            { Ast.Return }
    | TURNLEFT                                          { Ast.TurnLeft }
    | TURNOFF                                           { Ast.TurnOff }

if_statement:
    | IF; LPAREN; cond = bool_expr; RPAREN; i = statement %prec ELSE            { Ast.If (cond, i, Ast.Block []) }
    | IF; LPAREN; cond = bool_expr; RPAREN; i = statement; ELSE; e = statement  { Ast.If (cond, i, e) }    

while_statement:
    | WHILE; LPAREN; cond = bool_expr; RPAREN; b = statement           { Ast.While (cond, b) }

iterate_statement:
    | ITERATE; LPAREN; i = int_expr; RPAREN; b = statement             { Ast.Iterate (i, b) }

call:
    | ID; LPAREN; RPAREN                                { Ast.Call ($1, None) }
    | ID; LPAREN; i = int_expr; RPAREN                      { Ast.Call ($1, Some i) }

bool_expr:
    | b = bool_fun | b = bool_fun; LPAREN; RPAREN           { b }
    | IS_ZERO; LPAREN; i = int_expr; RPAREN                 { Ast.IsZero i }
    | e1=bool_expr; AND; e2=bool_expr                         { Ast.And (e1, e2) }
    | e1=bool_expr; OR; e2=bool_expr                          { Ast.Or (e1, e2) }
    | NOT; e=bool_expr                                    { Ast.Not e }
    | LPAREN; e = bool_expr; RPAREN                         { e }

bool_fun:
    | FRONT_IS_CLEAR                                     { Ast.FrontIsClear }
    | FRONT_IS_BLOCKED                                   { Ast.Not (FrontIsClear) }
    | LEFT_IS_CLEAR                                      { Ast.LeftIsClear }
    | LEFT_IS_BLOCKED                                    { Ast.Not (LeftIsClear) }
    | RIGHT_IS_CLEAR                                     { Ast.RightIsClear }
    | RIGHT_IS_BLOCKED                                   { Ast.Not (RightIsClear) }
    | FACING_NORTH                                       { Ast.FacingNorth }
    | NOT_FACING_NORTH                                   { Ast.Not (FacingNorth) }
    | FACING_SOUTH                                       { Ast.FacingSouth }
    | NOT_FACING_SOUTH                                   { Ast.Not (FacingSouth) }
    | FACING_EAST                                        { Ast.FacingEast }
    | NOT_FACING_EAST                                    { Ast.Not (FacingEast) }
    | FACING_WEST                                        { Ast.FacingWest }
    | NOT_FACING_WEST                                    { Ast.Not (FacingWest) }
    | ANY_BEEPERS_IN_BEEPER_BAG                          { Ast.AnyBeepersInBeeperBag }
    | NOT_ANY_BEEPERS_IN_BEEPER_BAG                      { Ast.Not (AnyBeepersInBeeperBag) }
    | NEXT_TO_A_BEEPER                                   { Ast.NextToABeeper }
    | NOT_NEXT_TO_A_BEEPER                               { Ast.Not (NextToABeeper) }

int_expr:
    | i = INT                                               { Ast.Int i }
    | i = ID                                                { Ast.Var i }
    | LPAREN; i = int_expr; RPAREN                          { i }
    | SUCC; LPAREN; i = int_expr; RPAREN                    { Ast.Succ i }
    | PRED; LPAREN; i = int_expr; RPAREN                    { Ast.Pred i }
