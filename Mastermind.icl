module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -> Int
positionalMatches [] _ = 0
positionalMatches _ [] = 0
positionalMatches [x:xs] [y:ys]
	| x == y = 1 + positionalMatches xs ys
	| otherwise = positionalMatches xs ys


Start = positionalMatches [1,2,3] [1,3,3]