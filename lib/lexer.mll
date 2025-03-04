(** File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception InvalidToken
}
rule token = parse
    | [' ' '\t']+   { token lexbuf }     (* skip blanks *)
    | '\n'          { Lexing.new_line lexbuf; token lexbuf }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "class"           { CLASS }
    | "program"         { PROGRAM }
    | "void" | "define" { VOID }
    | "if"              { IF }
    | "else"            { ELSE }
    | "while"           { WHILE }
    | "iterate"         { ITERATE }
    | ";"               { SEMICOLON }
    | "{"               { LBRACE }
    | "}"               { RBRACE }
    | '('               { LPAREN }
    | ')'               { RPAREN }
    | '!'               { NOT }
    | "&&"              { AND }
    | "||"              { OR }
    | "move"            { MOVE }
    | "pickbeeper"      { PICKBEEPER }
    | "putbeeper"       { PUTBEEPER }
    | "return"          { RETURN }
    | "turnleft"        { TURNLEFT }
    | "turnoff"         { TURNOFF }
    | "frontIsClear"    { FRONT_IS_CLEAR }
    | "frontIsBlocked"  { FRONT_IS_BLOCKED }
    | "leftIsClear"     { LEFT_IS_CLEAR }
    | "leftIsBlocked"   { LEFT_IS_BLOCKED }
    | "rightIsClear"    { RIGHT_IS_CLEAR }
    | "rightIsBlocked"  { RIGHT_IS_BLOCKED }
    | "facingEast"      { FACING_EAST }
    | "notFacingEast"   { NOT_FACING_EAST }
    | "facingNorth"     { FACING_NORTH }
    | "notFacingNorth"  { NOT_FACING_NORTH }
    | "facingSouth"     { FACING_SOUTH }
    | "notFacingSouth"  { NOT_FACING_SOUTH }
    | "facingWest"      { FACING_WEST }
    | "notFacingWest"   { NOT_FACING_WEST }
    | "anyBeepersInBeeperBag"       { ANY_BEEPERS_IN_BEEPER_BAG }
    | "notAnyBeepersInBeeperBag"    { NOT_ANY_BEEPERS_IN_BEEPER_BAG }
    | "nextToABeeper"               { NEXT_TO_A_BEEPER }
    | "notNextToABeeper"            { NOT_NEXT_TO_A_BEEPER }
    | "succ"            { SUCC }
    | "pred"            { PRED }
    | "iszero"          { IS_ZERO }
    | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm  { ID lxm }
    | eof               { EOF }
    | _                 { raise InvalidToken }
