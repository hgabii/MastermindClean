module Mastermind

import StdEnv, StdLib

/* Defined Functions */

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

maybe :: (a -> b) b (Maybe a) -> b
maybe converter defaultValue value
	| isNothing value = defaultValue
	| otherwise = converter(fromJust value)

allMatches :: [Int] String -> (Int, Int)
allMatches puzzle tip = 
	maybe (\validetedTipValue -> ( (matches puzzle validetedTipValue) - (positionalMatches puzzle validetedTipValue), positionalMatches puzzle validetedTipValue)) (0, 0) validatedTip
		where validatedTip = readCode tip

/* Enf of Defined Functions */

/* Entry point */
Start = [positionalMatches_test, matches_test, readCode_test, maybe_test, allMatches_test]

/* Test Cases */

positionalMatches_test =
	[ positionalMatches [4,2,7,1] [1,2,3,4] == 1
	, positionalMatches [9,3,0,5] [5,6,7,8] == 0
	, positionalMatches [6,6,6,1] [6,6,5,1] == 3
	]
	
matches_test =
	[ matches [4,2,7,1] [1,2,3,4] == 3
	, matches [9,3,0,5] [5,6,7,8] == 1
	, matches [6,6,6,1] [6,6,5,1] == 3
	, matches [5,8,7,9] [9,9,7,8] == 3
	]
	
readCode_test =
	[ readCode "1234" == Just [1,2,3,4]
	, readCode "12345" == Nothing
	, readCode "123a" == Nothing
	]
	
maybe_test =
	[ maybe ((+) 10) 7 Nothing == 7
	, maybe ((+) 10) 7 (Just 5) == 15
	]
	
allMatches_test =
	[ allMatches [4,2,7,1] "1234" == (2, 1)
	, allMatches [9,3,0,5] "1234" == (1, 0)
	, allMatches [9,3,0,5] "123a" == (0, 0)
	]

/* End of Test Cases */








