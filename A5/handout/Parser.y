{
module Parser where

import EDMinML
import Data.Char

}

%name parser
%tokentype { Token }

%token 
    num     { TokenInt $$ }
    boolean { TokenBool $$ }
    id      { TokenId $$ }
    '='	    { TokenEq }
    '+'	    { TokenPlus }
    '-'	    { TokenMinus }
    '*'	    { TokenTimes }
    '('	    { TokenOB }
    ')'	    { TokenCB }
    ','     { TokenComma }
    ':'     { TokenColon }
    if      { TokenIf }
    then    { TokenThen }
    else    { TokenElse }
    fun     { TokenFun }
    end     { TokenEnd }
    check   { TokenCheck }
    int     { TokenIntTy }
    bool    { TokenBoolTy }
    arr     { TokenArrow }

%%

Exp :: { Exp } 
  : if Exp then Exp else Exp	                      { If $2 $4 $6 }
  | fun id '(' id ':' Typ ')' ':' Typ '=' Exp end     { Fun $2 $4 $6 $9 $11 }
  | fun id id '=' Exp end                             { UTFun $2 $3 $5 }
  | check '(' Exp ',' Typ ')'                         { Check $3 $5 }      
  | BoolExp			                      { $1 }

BoolExp :: { Exp }
  : PlusExp '=' BoolExp { PrimOp Equal [$1,$3] }
  | PlusExp             { $1 }

PlusExp :: { Exp }  
  : MultExp '+' PlusExp		{ PrimOp Plus [$1,$3] }
  | MultExp '-' PlusExp		{ PrimOp Minus [$1,$3] }
  | MultExp			{ $1 }

MultExp :: { Exp }
  : NegExp '*' MultExp	{ PrimOp Times [$1,$3] }
  | NegExp		{ $1 }
    
NegExp :: { Exp }
  : '-' AppExp { PrimOp Negate [$2] }
  | AppExp     { $1 }
     
AppExp :: { Exp }
  : PrimExp '(' Exp ')' { Apply $1 $3 }
  | PrimExp             { $1 }

PrimExp :: { Exp }
  : num			{ Int $1 }
  | boolean             { Bool $1 }  
  | id			{ Var $1 }
  | '(' Exp ')'		{ $2 }

Typ :: { Typ }
  : PrimTyp arr Typ { ARROW $1 $3 }
  | PrimTyp         { $1 }

PrimTyp :: { Typ }
  : int   { INT }
  | bool  { BOOL }
  | '(' Typ ')' { $2 }

{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Token =
    TokenInt Int
  | TokenBool Bool
  | TokenId String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenOB
  | TokenCB
  | TokenComma
  | TokenColon
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFun
  | TokenEnd
  | TokenCheck
  | TokenIntTy
  | TokenBoolTy
  | TokenArrow

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
  | isSpace c = lexer cs
  | isAlpha c = lexId (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (':':cs) = TokenColon : lexer cs

lexNum :: String -> [Token]
lexNum cs = TokenInt (read num) : lexer rest
	where (num,rest) = span isDigit cs

lexId :: String -> [Token]
lexId cs =
  case span isAlpha cs of
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest)  -> TokenThen : lexer rest
    ("else",rest)  -> TokenElse : lexer rest
    ("fun",rest)  -> TokenFun : lexer rest
    ("end",rest)  -> TokenEnd : lexer rest
    ("check",rest)  -> TokenCheck : lexer rest
    ("int",rest)  -> TokenIntTy : lexer rest
    ("bool",rest)  -> TokenBoolTy : lexer rest
    ("true",rest) -> TokenBool True : lexer rest
    ("false",rest) -> TokenBool False : lexer rest
    (id,rest)   -> TokenId id : lexer rest


}