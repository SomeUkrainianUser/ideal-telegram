module Parser where

import Tokenizer
import Data.String

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

keywords = ["integer", "floating", "character", "boolean", "void", "unsigned", "signed", "immutable", "mutable",
            "byte", "half", "word", "double", "args"]

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

parseParens :: [Token] -> Int -> Int -> [Token]
parseParens [] _ _ = []
parseParens toks (-1) x = TokenParens (drop 1 (init (take x toks))) : drop x toks
parseParens toks depth len
    | openCount > closeCount = error "Error: unmatched opening parenthesis"
    | openCount < closeCount = error "Error: unmatched closing parenthesis"
    | depth == 0 && len /= 0 = parseParens toks (-1) len 
    | not (null nextPar) && take 1 nextPar == [TokenLPar] = parseParens toks (depth + 1) (len + length prePar + 1)
    | not (null nextPar) && take 1 nextPar == [TokenRPar] = parseParens toks (depth - 1) (len + length prePar + 1)
        where
            openCount = length (filter (== TokenLPar) toks)
            closeCount = length (filter (== TokenRPar) toks)
            (prePar, nextPar) = span (`notElem` [TokenLPar, TokenRPar]) (drop len toks)
    

parseExpression :: [Token] -> (Expression, [Token])
parseExpression [TokenParens x] = parseExpression x
parseExpression [TokenValue x] = (Number x, [])
parseExpression (TokenValue x : TokenSemicolon : remainder) = (Number x, remainder)
parseExpression (TokenValue x : TokenRPar : remainder) = (Number x, remainder)
parseExpression toks
    | not (null par) = parseExpression (prePar ++ parseParens par 0 0)
    | not (null rPar) = error "Error: unmatched closing parenthesis"
    | containsPlus = let (pLhsNode, _) = parseExpression pLhs
                         (pRhsNode, _) = parseExpression pRhs
                    in (Binary Plus pLhsNode pRhsNode, remainderPostExpr)
    | containsMinus = let (mLhsNode, _) = parseExpression mLhs
                          (mRhsNode, _) = parseExpression mRhs
                    in (Binary Minus mLhsNode mRhsNode, remainderPostExpr)
    | containsGreater = let (gLhsNode, _) = parseExpression gLhs
                            (gRhsNode, _) = parseExpression gRhs
                    in (Binary Greater gLhsNode gRhsNode, remainderPostExpr)
    | containsLess = let (lLhsNode, _) = parseExpression lLhs
                         (lRhsNode, _) = parseExpression lRhs
                    in (Binary Less lLhsNode lRhsNode, remainderPostExpr)
    | containsGreaterEqual = let (geLhsNode, _) = parseExpression geLhs
                                 (geRhsNode, _) = parseExpression geRhs
                    in (Binary GreaterEqual geLhsNode geRhsNode, remainderPostExpr)
    | containsLessEqual = let (leLhsNode, _) = parseExpression leLhs
                              (leRhsNode, _) = parseExpression leRhs
                    in (Binary LessEqual leLhsNode leRhsNode, remainderPostExpr)
    | containsEqual = let (eLhsNode, _) = parseExpression eLhs
                          (eRhsNode, _) = parseExpression eRhs
                    in (Binary Equal eLhsNode eRhsNode, remainderPostExpr)
    | containsNotEqual = let (neLhsNode, _) = parseExpression neLhs
                             (neRhsNode, _) = parseExpression neRhs
                    in (Binary NotEqual neLhsNode neRhsNode, remainderPostExpr)
    | containsAnd = let (aLhsNode, _) = parseExpression aLhs
                        (aRhsNode, _) = parseExpression aRhs
                    in (Binary And aLhsNode aRhsNode, remainderPostExpr)
    | containsOr = let (oLhsNode, _) = parseExpression oLhs
                       (oRhsNode, _) = parseExpression oRhs
                    in (Binary Or oLhsNode oRhsNode, remainderPostExpr)
    | containsXor = let (xLhsNode, _) = parseExpression xLhs
                        (xRhsNode, _) = parseExpression xRhs
                    in (Binary Xor xLhsNode xRhsNode, remainderPostExpr)
    | containsNand = let (naLhsNode, _) = parseExpression naLhs
                         (naRhsNode, _) = parseExpression naRhs
                    in (Binary Nand naLhsNode naRhsNode, remainderPostExpr)
    | containsNor = let (noLhsNode, _) = parseExpression noLhs
                        (noRhsNode, _) = parseExpression noRhs
                    in (Binary Nor noLhsNode noRhsNode, remainderPostExpr)
    | containsXnor = let (xnLhsNode, _) = parseExpression xnLhs
                         (xnRhsNode, _) = parseExpression xnRhs
                    in (Binary Xnor xnLhsNode xnRhsNode, remainderPostExpr)
    | containsBAnd = let (baLhsNode, _) = parseExpression baLhs
                         (baRhsNode, _) = parseExpression baRhs
                    in (Binary BitwiseAnd baLhsNode baRhsNode, remainderPostExpr)
    | containsBOr = let (boLhsNode, _) = parseExpression boLhs
                        (boRhsNode, _) = parseExpression boRhs
                    in (Binary BitwiseOr boLhsNode boRhsNode, remainderPostExpr)
    | containsBXor = let (bxLhsNode, _) = parseExpression bxLhs
                         (bxRhsNode, _) = parseExpression bxRhs
                    in (Binary BitwiseXor bxLhsNode bxRhsNode, remainderPostExpr)
    | containsBNand = let (bnaLhsNode, _) = parseExpression bnaLhs
                          (bnaRhsNode, _) = parseExpression bnaRhs
                    in (Binary BitwiseNand bnaLhsNode bnaRhsNode, remainderPostExpr)
    | containsBNor = let (bnoLhsNode, _) = parseExpression bnoLhs
                         (bnoRhsNode, _) = parseExpression bnoRhs
                    in (Binary BitwiseNor bnoLhsNode bnoRhsNode, remainderPostExpr)
    | containsBXnor = let (bxnLhsNode, _) = parseExpression bxnLhs
                          (bxnRhsNode, _) = parseExpression bxnRhs
                    in (Binary BitwiseXnor bxnLhsNode bxnRhsNode, remainderPostExpr)
    | containsMul = let (mulLhsNode, _) = parseExpression mulLhs
                        (mulRhsNode, _) = parseExpression mulRhs
                    in (Binary Star mulLhsNode mulRhsNode, remainderPostExpr)
    | containsDiv = let (dLhsNode, _) = parseExpression dLhs
                        (dRhsNode, _) = parseExpression dRhs
                    in (Binary Slash dLhsNode dRhsNode, remainderPostExpr)
    | otherwise = error "Expression parsing error"
        where
            reverseTuple :: (a, a) -> (a, a)
            reverseTuple (x, y) = (y, x)

            (exprTok, remainderPostExpr) = span (/= TokenSemicolon) toks

            (prePar, par) = span (/= TokenLPar) exprTok
            (_, rPar) = span (/= TokenRPar) exprTok

            (prRhs, sprLhs) = span (/= TokenPlus) (reverse toks)
            (_:prLhs) = sprLhs
            containsPlus = not (null sprLhs)
            pRhs = reverse prRhs
            pLhs = reverse prLhs

            (mrRhs, smrLhs) = span (/= TokenMinus) (reverse toks)
            (_:mrLhs) = smrLhs
            containsMinus = not (null smrLhs)
            mRhs = reverse mrRhs
            mLhs = reverse mrLhs

            (grRhs, sgrLhs) = span (/= TokenGreaterThan) (reverse toks)
            (_:grLhs) = sgrLhs
            containsGreater = not (null sgrLhs)
            gRhs = reverse grRhs
            gLhs = reverse grLhs

            (lrRhs, slrLhs) = span (/= TokenLessThan) (reverse toks)
            (_:lrLhs) = slrLhs
            containsLess = not (null slrLhs)
            lRhs = reverse lrRhs
            lLhs = reverse lrLhs

            (gerRhs, sgerLhs) = span (/= TokenGreaterOrEqualTo) (reverse toks)
            (_:gerLhs) = sgerLhs
            containsGreaterEqual = not (null sgerLhs)
            geRhs = reverse gerRhs
            geLhs = reverse gerLhs

            (lerRhs, slerLhs) = span (/= TokenLessOrEqualTo) (reverse toks)
            (_:lerLhs) = slerLhs
            containsLessEqual = not (null slerLhs)
            leRhs = reverse lerRhs
            leLhs = reverse lerLhs

            (erRhs, serLhs) = span (/= TokenEqual) (reverse toks)
            (_:erLhs) = serLhs
            containsEqual = not (null serLhs)
            eRhs = reverse erRhs
            eLhs = reverse erLhs

            (nerRhs, snerLhs) = span (/= TokenNotEqual) (reverse toks)
            (_:nerLhs) = snerLhs
            containsNotEqual = not (null snerLhs)
            neRhs = reverse nerRhs
            neLhs = reverse nerLhs

            (arRhs, sarLhs) = span (/= TokenAnd) (reverse toks)
            (_:arLhs) = sarLhs
            containsAnd = not (null sarLhs)
            aRhs = reverse arRhs
            aLhs = reverse arLhs

            (orRhs, sorLhs) = span (/= TokenOr) (reverse toks)
            (_:orLhs) = sorLhs
            containsOr = not (null sorLhs)
            oRhs = reverse orRhs
            oLhs = reverse orLhs

            (xrRhs, sxrLhs) = span (/= TokenXor) (reverse toks)
            (_:xrLhs) = sxrLhs
            containsXor = not (null sxrLhs)
            xRhs = reverse xrRhs
            xLhs = reverse xrLhs

            (narRhs, snarLhs) = span (/= TokenNand) (reverse toks)
            (_:narLhs) = snarLhs
            containsNand = not (null snarLhs)
            naRhs = reverse narRhs
            naLhs = reverse narLhs

            (norRhs, snorLhs) = span (/= TokenNor) (reverse toks)
            (_:norLhs) = snorLhs
            containsNor = not (null snorLhs)
            noRhs = reverse norRhs
            noLhs = reverse norLhs

            (xnrRhs, sxnrLhs) = span (/= TokenXnor) (reverse toks)
            (_:xnrLhs) = sxnrLhs
            containsXnor = not (null sxnrLhs)
            xnRhs = reverse xnrRhs
            xnLhs = reverse xnrLhs


            (barRhs, sbarLhs) = span (/= TokenAmpersand) (reverse toks)
            (_:barLhs) = sbarLhs
            containsBAnd = not (null sbarLhs)
            baRhs = reverse barRhs
            baLhs = reverse barLhs

            (borRhs, sborLhs) = span (/= TokenPipe) (reverse toks)
            (_:borLhs) = sborLhs
            containsBOr = not (null sborLhs)
            boRhs = reverse borRhs
            boLhs = reverse borLhs

            (bxrRhs, sbxrLhs) = span (/= TokenCaret) (reverse toks)
            (_:bxrLhs) = sbxrLhs
            containsBXor = not (null sbxrLhs)
            bxRhs = reverse bxrRhs
            bxLhs = reverse bxrLhs

            (bnarRhs, sbnarLhs) = span (/= TokenBitwiseNand) (reverse toks)
            (_:bnarLhs) = sbnarLhs
            containsBNand = not (null sbnarLhs)
            bnaRhs = reverse bnarRhs
            bnaLhs = reverse bnarLhs

            (bnorRhs, sbnorLhs) = span (/= TokenBitwiseNor) (reverse toks)
            (_:bnorLhs) = sbnorLhs
            containsBNor = not (null sbnorLhs)
            bnoRhs = reverse bnorRhs
            bnoLhs = reverse bnorLhs

            (bxnrRhs, sbxnrLhs) = span (/= TokenBitwiseXnor) (reverse toks)
            (_:bxnrLhs) = sbxnrLhs
            containsBXnor = not (null sxnrLhs)
            bxnRhs = reverse bxnrRhs
            bxnLhs = reverse bxnrLhs
            
            (mulrRhs, smulrLhs) = span (/= TokenStar) (reverse toks)
            containsMul = not (null smulrLhs)
            (_:mulrLhs) = smulrLhs
            mulRhs = reverse mulrRhs
            mulLhs = reverse mulrLhs

            (drRhs, sdrLhs) = span (/= TokenSlash) (reverse toks)
            (_:drLhs) = sdrLhs
            containsDiv = not (null sdrLhs)
            dRhs = reverse drRhs
            dLhs = reverse drLhs


parseVariableDeclaration :: [Token] -> (VariableDeclaration, [Token])
parseVariableDeclaration toks
    | null remainderPostType = error "Expected an ID, but received EOF"
    | null remainderPostName = error "Expected a semicolon or an initialization, but received EOF"
    | not (isIDToken name)  = error ("Expected an ID, but received " ++ show name)
    | isKeyword x = error ("Expected an ID, but received keyword '" ++ x ++ "'")
    | end `notElem` [TokenSemicolon, TokenAssign] = error ("Expected a semicolon or an initialization, but received " ++ show end)
    | end == TokenSemicolon = (VariableDeclare (Variable varType x), remainderPostEnd)
    | null remainderPostEnd = error "Expected an expression, but received EOF"
    | null remainderPostExpr = error "Expected a semicolon, but received EOF"
    | postExprEnd /= TokenSemicolon = error ("Expected a semicolon, but received " ++ show postExprEnd)
    | otherwise = (VariableInit (Variable varType x) expr, remainder)
        where
            (varType, remainderPostType) = parseType toks
            (name:remainderPostName) = remainderPostType
            (end:remainderPostEnd) = remainderPostName
            (expr, remainderPostExpr) = parseExpression remainderPostEnd
            (postExprEnd:remainder) = remainderPostExpr

            TokenID x = name

            isIDToken :: Token -> Bool
            isIDToken (TokenID _) = True
            isIDToken _ = False

            isKeyword :: String -> Bool
            isKeyword x = x `elem` keywords



parseStatement :: [Token] -> (Statement, [Token])
parseStatement (TokenID x : TokenID y : lst) = (Block [], [])

parseBlock :: [Token] -> Block
parseBlock [] = []
parseBlock (TokenRCurly:_) = []
parseBlock l = st : parse remainder
    where
        (st, remainder) = parseStatement l

parse :: [Token] -> Block
parse = parseBlock