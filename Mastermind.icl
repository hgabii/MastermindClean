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

stringToList :: String -> [Char]
stringToList str = [ char \\ char <-: str ]

readCode :: String -> Maybe [Int]
readCode str
	| length (strArray) == 4 && and (map isDigit strArray) = Just (map digitToInt strArray)
	| otherwise = Nothing
		where
			strArray = stringToList str

//isDigit
//digitToInt

maybe :: (a -> b) b (Maybe a) -> b
maybe converter defaultValue value
	| isNothing value = defaultValue
	| otherwise = converter(fromJust value)


Start = maybe ((+) 10) 7 (Just 5)