module Parser where

import Tokenizer

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

data Type = MakeType BaseType [Modifier] Int deriving (Show, Eq)

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