module Mastermind

import StdEnv, StdLib

positionalMatches :: [Int] [Int] -> Int
positionalMatches [] _ = 0
positionalMatches _ [] = 0
positionalMatches [x:xs] [y:ys]
	| x == y = 1 + positionalMatches xs ys
	| otherwise = positionalMatches xs ys
	
matches :: [Int] [Int] -> Int
matches firstList secondList = matchesForSorted (sort firstList) (sort secondList)
	where
		matchesForSorted [] _ = 0
		matchesForSorted _ [] = 0
		matchesForSorted [x:xs] [y:ys]
			| x < y = matchesForSorted xs [y:ys]
			| y < x = matchesForSorted [x:xs] ys
			| x == y = 1 + matchesForSorted xs ys


Start = matches [6,6,6,1] [6,6,5,1]