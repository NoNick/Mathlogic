module Main where

import qualified Data.ByteString as B
import Text.Parsec
import Expression
import Parse
import Data.List

main = do file <- B.readFile "input.txt"
          case parse pFile "" file of
            (Right (h, p)) -> (do let hText = (intercalate ", " $ map show (fst h)) ++ " |- " ++ show (snd h)
                                  let pText = (intercalate "\n" $ map show p)
                                  writeFile "output.txt" (hText ++ "\n" ++ pText ++ "\n"))
            (Left e)       -> putStrLn $ show e
