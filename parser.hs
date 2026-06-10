module Parser where

import Tokenizer

data UnaryOperator = Not
                    | Pointer
                    | Dereference
                    deriving (Show, Eq)

data UnaryOperation = UnaryOperation UnaryOperator Expression deriving (Show, Eq)

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
                       

data BinaryOperation = BinaryOperation BinaryOperator Expression Expression deriving (Show, Eq)

data Expression = Number Int
                 | Var Variable
                 | Unary UnaryOperation
                 | Binary BinaryOperation
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

data Type = MakeType BaseType [Modifier] Int deriving (Show, Eq)

data Variable = Variable Type String deriving (Show, Eq)

data VariableDeclaration = VariableInit Variable Expression
                          | VariableDeclare Variable
                          deriving (Show, Eq)