module Parse where

import qualified Data.ByteString as B
import Expression
import Text.Parsec

pVar :: Parsec B.ByteString () String
pVar = do x  <- oneOf $ ['a' .. 'z']
          xs <- many $ oneOf $ ['0' .. '9']
          return $ x:xs

pName :: Parsec B.ByteString () String
pName = do x  <- oneOf $ ['A' .. 'Z']
           xs <- many $ oneOf $ ['0' .. '9']
           return $ x:xs  
           
pExpr :: Parsec B.ByteString () Expr
pExpr = do a <- pDisj
           e <- try (do string "->"
                        b <- pExpr
                        return $ Imply a b) <|>
                return a
           return e

pDisj :: Parsec B.ByteString () Expr
pDisj = do x <- pConj
           e <- try (do xs <- many1 (char '|' >> pConj)
                        return $ Disj (x:xs)) <|>
                return x
           return e
        
pConj :: Parsec B.ByteString () Expr
pConj = do x <- pUnary
           e <- try (do xs <- many1 (char '&' >> pUnary)
                        return $ Conj (x:xs)) <|>
                return x
           return e

pUnary :: Parsec B.ByteString () Expr
pUnary = try (char '!' >> pUnary >>= return . Not) <|>
         try (do char '('
                 b <- pExpr
                 char ')'
                 return $ BracketsE b) <|>
         try (do char '@'
                 v <- pVar
                 u <- pUnary
                 return $ Forall v u) <|>
         try (do char '?'
                 v <- pVar
                 u <- pUnary
                 return $ Exists v u) <|>
         try (pPred >>= return . Predicate)

pPred :: Parsec B.ByteString () Pred
pPred = try (do name <- pName
                args <- try (do char '('
                                arg1 <- pTerm
                                rest <- many $ try $ char ',' >> pTerm
                                char ')'
                                return $ arg1:rest) <|> (return [])
                return $ Custom name args) <|>
        try (do t1 <- pTerm
                char '='
                t2 <- pTerm
                return $ Equals t1 t2)
                       
pTerm :: Parsec B.ByteString () Term
pTerm = try (do first <- pSummand
                rest <- many $ char '+' >> pSummand
                return $ Sum (first:rest))

pSummand :: Parsec B.ByteString () Term
pSummand = try (do first <- pInc
                   rest <- many $ char '*' >> pInc
                   return $ Mult (first:rest))
                           
pInc :: Parsec B.ByteString () Term
pInc = try (do m <- pMult
               c <- many $ char '\''
               return $ inc m (length c))

inc :: Term -> Int -> Term
inc t 0 = t
inc t n = inc (Inc t) (pred n)

pMult :: Parsec B.ByteString () Term
pMult = try (char '0' >> return Zero) <|>
        try (do char '('
                b <- pTerm
                char ')'
                return $ BracketsT b) <|>
        try (do v <- pVar
                char '('
                first <- pTerm
                rest <- many (char ',' >> pTerm)
                char ')'
                return $ Func v (first:rest)) <|>
        (pVar >>= return . Var)

pHeader :: Parsec B.ByteString () Header
pHeader = do p <- many $ try (do e <- pExpr
                                 char ','
                                 return e)
             p' <- pExpr
             string "|-"
             e <- pExpr
             return ((p ++ [p']), e)

pProof :: Parsec B.ByteString () Proof
pProof = do x <- pExpr
            xs <- many $ try (char '\n' >> pExpr)
            return $ x:xs

pFile :: Parsec B.ByteString () (Header, Proof)
pFile = do h <- pHeader
           char '\n'
           p <- pProof
           return (h, p)