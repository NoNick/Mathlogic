module Parser where

import Text.Parsec
import Ordinal

sumOrd :: [Ordinal] -> Ordinal
sumOrd (x:y:[]) = Sum y x
sumOrd (x:xs) = Sum (sumOrd xs) x
    
pSum :: Parsec String () Ordinal
pSum = do x <- pMult
          xs <- many (char '+' >> pMult)
          case xs of
            [] -> return x
            _  -> return $ sumOrd $ reverse (x:xs)

mulOrd :: [Ordinal] -> Ordinal
mulOrd (x:y:[]) = Mult y x
mulOrd (x:xs) = Mult (mulOrd xs) x
          
pMult :: Parsec String () Ordinal
pMult = do x <- pPow
           xs <- many (char '*' >> pPow)
           case xs of
             [] -> return x
             _  -> return $ mulOrd $ reverse (x:xs)

powOrd :: [Ordinal] -> Ordinal
powOrd (x:[]) = x
powOrd (x:xs) = Pow x (powOrd xs)

pPow :: Parsec String () Ordinal
pPow = do x <- pTerm
          xs <- many (char '^' >> pTerm)
          case xs of
            [] -> return x
            _  -> return $ powOrd (x:xs)

pTerm :: Parsec String () Ordinal
pTerm = (char 'w' >> return Omega) <|>
        (do char '('
            res <- pSum
            char ')'
            return res) <|>
        (many digit >>= return . Num . read)

pEq :: Parsec String () (Ordinal, Ordinal)
pEq = do o1 <- pSum
         many $ char ' '
         char '='
         many $ char ' '              
         o2 <- pSum
         return (o1, o2)
