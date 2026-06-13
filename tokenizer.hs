module Tokenizer where

import Data.Char(isDigit)
data Token = TokenValue Int
            | TokenID String
            | TokenString String
            | TokenEqual
            | TokenGreaterThan
            | TokenLessThan
            | TokenGreaterOrEqualTo
            | TokenLessOrEqualTo
            | TokenNotEqual
            | TokenAnd
            | TokenOr
            | TokenXor
            | TokenNot
            | TokenNand
            | TokenNor
            | TokenXnor
            | TokenAmpersand
            | TokenPipe
            | TokenCaret
            | TokenTilde
            | TokenBitwiseNand
            | TokenBitwiseNor
            | TokenBitwiseXnor
            | TokenPlus
            | TokenMinus
            | TokenStar
            | TokenSlash
            | TokenAt
            | TokenDollarSign
            | TokenAssign
            | TokenLPar
            | TokenRPar
            | TokenLCurly
            | TokenRCurly
            | TokenSemicolon
            | TokenColon
            | TokenComma
            | TokenArrow
            | TokenParens [Token]
            | TokenBlock [Token]
            deriving(Eq, Show)

isValidIDChar :: Char -> Bool
isValidIDChar c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_', '\''])



tokenize :: String -> [Token]
tokenize "" = []
tokenize src
    | isDigit h = TokenValue (read num) : tokenize (drop (length num) src)
    | isValidIDChar h = TokenID id : tokenize (drop (length id) src)
    | h == '"' = TokenString (h : str ++ ['"']) : tokenize (drop (length str + 2) src) -- padding for quotes

    | prefix2 == "==" = TokenEqual              : tokenize pp2src
    | prefix2 == ">=" = TokenGreaterOrEqualTo   : tokenize pp2src
    | prefix2 == "<=" = TokenLessOrEqualTo      : tokenize pp2src
    | prefix2 == "/=" = TokenNotEqual           : tokenize pp2src
    | prefix2 == "&&" = TokenAnd                : tokenize pp2src
    | prefix2 == "||" = TokenOr                 : tokenize pp2src
    | prefix2 == "^^" = TokenXor                : tokenize pp2src
    | prefix2 == "!&" = TokenNand               : tokenize pp2src
    | prefix2 == "!|" = TokenNor                : tokenize pp2src
    | prefix2 == "!^" = TokenXnor               : tokenize pp2src
    | prefix2 == "~&" = TokenBitwiseNand        : tokenize pp2src
    | prefix2 == "~|" = TokenBitwiseNor         : tokenize pp2src
    | prefix2 == "~^" = TokenBitwiseXnor        : tokenize pp2src
    | prefix2 == "->" = TokenArrow              : tokenize pp2src

    | prefix1 == ">"  = TokenGreaterThan        : tokenize pp1src 
    | prefix1 == "<"  = TokenLessThan           : tokenize pp1src
    | prefix1 == "&"  = TokenAmpersand          : tokenize pp1src
    | prefix1 == "|"  = TokenPipe               : tokenize pp1src
    | prefix1 == "^"  = TokenCaret              : tokenize pp1src
    | prefix1 == "~"  = TokenTilde              : tokenize pp1src
    | prefix1 == "+"  = TokenPlus               : tokenize pp1src 
    | prefix1 == "-"  = TokenMinus              : tokenize pp1src
    | prefix1 == "*"  = TokenStar               : tokenize pp1src
    | prefix1 == "/"  = TokenSlash              : tokenize pp1src
    | prefix1 == "@"  = TokenAt                 : tokenize pp1src
    | prefix1 == "$"  = TokenDollarSign         : tokenize pp1src
    | prefix1 == "="  = TokenAssign             : tokenize pp1src
    | prefix1 == "("  = TokenLPar               : tokenize pp1src
    | prefix1 == ")"  = TokenRPar               : tokenize pp1src
    | prefix1 == "{"  = TokenLCurly             : tokenize pp1src
    | prefix1 == "}"  = TokenRCurly             : tokenize pp1src
    | prefix1 == ";"  = TokenSemicolon          : tokenize pp1src
    | prefix1 == ":"  = TokenColon              : tokenize pp1src
    | prefix1 == ","  = TokenComma              : tokenize pp1src

    | prefix1 `elem` whitespaces =                tokenize pp1src
    | otherwise       = error "Bad token"
        where
            num = takeWhile isDigit src
            id = takeWhile isValidIDChar src
            str = takeWhile (/= '"') pp1src
            (h:_) = src
            prefix2 = take 2 src
            pp2src = drop 2 src
            prefix1 = take 1 src
            pp1src = drop 1 src
            whitespaces = ["\t", " ", "\r"]