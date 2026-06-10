module Parser where

import Tokenizer
import Distribution.Compat.Prelude (ExitCode, exitWith)

data UnaryOperator = Not
                    | Pointer
                    | Dereference
                    deriving (Show, Eq)

data BinaryOperator =  Plus 
                       | Minus
                       | Star
                       | Slash
                       | Greater
                       | Less
                       | GreaterEqual
                       | LessEqual
                       | And
                       | Or
                       | Xor
                       | Nand
                       | Nor
                       | Xnor
                       | BitwiseAnd
                       | BitwiseOr
                       | BitwiseXor
                       | BitwiseNand
                       | BitwiseNor
                       | BitwiseXnor
                       | Equal
                       | NotEqual
                       | Assign
                       deriving (Show, Eq)
                       

data Expression = Number Int
                 | Var Variable
                 | Unary UnaryOperator Expression
                 | Binary BinaryOperator Expression Expression
                 deriving (Show, Eq)

data Modifier = Unsigned
                | Signed
                | Immutable
                | Mutable
                | Byte
                | Half
                | Word
                | Double
                deriving (Show, Eq)

data BaseType = Character | Integer | Floating | Boolean | Void deriving (Show, Eq)

data Type = Type BaseType [Modifier] Int deriving (Show, Eq)

data Variable = Variable Type String deriving (Show, Eq)

type Arguments = [Variable]

data Function = Function String Arguments Type (Maybe Block) deriving (Show, Eq)

data VariableDeclaration = VariableInit Variable Expression
                          | VariableDeclare Variable
                          deriving (Show, Eq)

type Block = [Statement]

data Statement = VarDecl VariableDeclaration
                | Block Block
                | FuncDecl Function
                deriving (Show, Eq)

isBaseType :: Token -> Bool
isBaseType (TokenID "integer") = True
isBaseType (TokenID "floating") = True
isBaseType (TokenID "character") = True
isBaseType (TokenID "boolean") = True
isBaseType (TokenID "void") = True
isBaseType _ = False

isMutabilityModifier :: Token -> Bool
isMutabilityModifier (TokenID "immutable") = True
isMutabilityModifier (TokenID "mutable") = True
isMutabilityModifier _ = False

isSignumModifier :: Token -> Bool
isSignumModifier (TokenID "unsigned") = True
isSignumModifier (TokenID "signed") = True
isSignumModifier _ = False

isSizeModifier :: Token -> Bool
isSizeModifier (TokenID "byte") = True
isSizeModifier (TokenID "half") = True
isSizeModifier (TokenID "word") = True
isSizeModifier (TokenID "double") = True
isSizeModifier _ = False

isTypeModifier :: Token -> Bool
isTypeModifier x = isMutabilityModifier x || isSignumModifier x || isSizeModifier x

isType :: Token -> Bool
isType t = isBaseType t || isTypeModifier t

tokToBaseType :: Token -> BaseType
tokToBaseType (TokenID "integer") = Integer
tokToBaseType (TokenID "floating") = Floating
tokToBaseType (TokenID "character") = Character
tokToBaseType (TokenID "boolean") = Boolean
tokToBaseType (TokenID "void") = Void

tokToTypeModifier :: Token -> Modifier
tokToTypeModifier (TokenID "immutable") = Immutable
tokToTypeModifier (TokenID "mutable") = Mutable
tokToTypeModifier (TokenID "unsigned") = Unsigned
tokToTypeModifier (TokenID "signed") = Signed
tokToTypeModifier (TokenID "byte") = Byte
tokToTypeModifier (TokenID "half") = Half
tokToTypeModifier (TokenID "word") = Word
tokToTypeModifier (TokenID "double") = Double

parseType :: [Token] -> (Type, [Token])
parseType toks
    | isBaseType h = error "Syntax error: base type used without specifiers\n"
    | not (isType h) = error ("Syntax error: expected type, but received " ++ show h)
    | szAmount /= 1 = error ("Syntax error: expected 1 size modifier, but received " ++ show szAmount)
    | sgAmount > 1 = error ("Syntax error: expected at most 1 sign modifier, but received " ++ show sgAmount)
    | mutAmount > 1 = error ("Syntax error: expected at most 1 mutability modifier, but received " ++ show mutAmount)
    | null remainder = error "Syntax error: expected base type after the modifiers, but received EOF"
    | not (isBaseType btToken ) = error ("Syntax error: expected base type after the modifiers, but received " ++ show btToken)
    | otherwise = (Type bt modifiers ptr_depth, remainderPostPtr)
        where
            (h:ts) = toks
            (modifiersTokens, remainder) = span isTypeModifier toks

            sizeModifier = filter isSizeModifier modifiersTokens
            szAmount = length sizeModifier
            signumModifier = filter isSignumModifier modifiersTokens
            sgAmount = length signumModifier
            mutModifier = filter isMutabilityModifier modifiersTokens
            mutAmount = length mutModifier

            btToken = head remainder
            bt = tokToBaseType btToken
            modifiers = map tokToTypeModifier modifiersTokens

            remainderPostType = drop 1 remainder
            (stars, remainderPostPtr) = span (== TokenStar) remainderPostType
            ptr_depth = length stars

parseStatement :: [Token] -> Either ExitCode (Statement, [Token])
parseStatement (TokenID x : TokenID y : lst) = Right (Block [], [])

parse :: [Token] -> Block
parse [] = []