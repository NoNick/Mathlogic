module Main where

import Text.Parsec
import Parser
import Ordinal
import CNF

main = do input <- readFile "input.txt"
          let out = case parse pEq "" input of
                      (Right (o1, o2)) -> if cmp (ord2CNF o1) (ord2CNF o2) == EQ then "Equal\n"
                                         else "Different\n"
                      (Left l) -> show l
          writeFile "output.txt" out

unpack (Right r) = r
p = parse pSum "" "(w+1)^2"
o = unpack p
p' = parse pSum "" "w^2+2*w+1"
o' = unpack p'
