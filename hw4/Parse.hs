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

disjFromList (x:y:[]) = Disj y x
disjFromList (x:xs) = Disj (disjFromList xs) x

pDisj :: Parsec B.ByteString () Expr
pDisj = do x <- pConj
           e <- try (do xs <- many1 (char '|' >> pConj)
                        return $ disjFromList $ reverse (x:xs)) <|>
                return x
           return e

conjFromList (x:y:[]) = Conj y x
conjFromList (x:xs) = Conj (disjFromList xs) x
                  
pConj :: Parsec B.ByteString () Expr
pConj = do x <- pUnary
           e <- try (do xs <- many1 (char '&' >> pUnary)
                        return $ conjFromList $ reverse (x:xs)) <|>
                return x
           return e

pUnary :: Parsec B.ByteString () Expr
pUnary = try (char '!' >> pUnary >>= return . Not) <|>
         try (do char '('
                 b <- pExpr
                 char ')'
                 return b) <|>
         try (do char '@'
                 v <- pVar
                 u <- pUnary
                 return $ Forall v u) <|>
         try (do char '?'
                 v <- pVar
                 u <- pUnary
                 return $ Exists v u) <|>
         pPred

pPred :: Parsec B.ByteString () Expr
pPred = try (do name <- pName
                args <- try (do char '('
                                arg1 <- pTerm
                                rest <- many $ try $ char ',' >> pTerm
                                char ')'
                                return $ arg1:rest) <|> (return [])
                return $ CustomP name args) <|>
        (do t1 <- pTerm
            char '='
            t2 <- pTerm
            return $ EqualsP t1 t2)
                       
pTerm :: Parsec B.ByteString () Expr
pTerm = do first <- pSummand
           rest <- many $ char '+' >> pSummand
           if null rest then return first
           else return $ Sum (first:rest)

pSummand :: Parsec B.ByteString () Expr
pSummand = do first <- pInc
              rest <- many $ char '*' >> pInc
              if null rest then return first
              else return $ Mult (first:rest)
                           
pInc :: Parsec B.ByteString () Expr
pInc = do m <- pMult
          c <- many $ char '\''
          return $ inc m (length c)

inc :: Expr -> Int -> Expr
inc t 0 = t
inc t n = inc (Inc t) (pred n)

pMult :: Parsec B.ByteString () Expr
pMult = try (char '0' >> return Zero) <|>
        try (do char '('
                b <- pTerm
                char ')'
                return b) <|>
        try (do v <- pVar
                char '('
                first <- pTerm
                rest <- many (char ',' >> pTerm)
                char ')'
                return $ Func v (first:rest)) <|>
        (pVar >>= return . Var)

pHeader :: Parsec B.ByteString () Header
pHeader = try (do string "|-"
                  e <- pExpr
                  return ([], e)) <|>
              (do p <- many $ try (do e <- pExpr
                                      char ','
                                      return e)
                  p' <- pExpr
                  string "|-"
                  e <- pExpr
                  return ((p ++ [p']), e))

pProof :: Parsec B.ByteString () Proof
pProof = do x <- pExpr
            xs <- many $ try ((many $ oneOf "\n\r\t ") >> pExpr)
            return $ x:xs

pFile :: Parsec B.ByteString () (Header, Proof)
pFile = do h <- pHeader
           many $ oneOf "\r\n"
           p <- pProof
           return (h, p)

