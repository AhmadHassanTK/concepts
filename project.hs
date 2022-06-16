import DataFile

-- 1) wordToken
-- > wordToken "the sun is shining. the wind is blowing"
-- ["the","sun","is","shining",".","the","wind","is","blowing"]
wordToken :: String -> [String]
wordToken x = words x

-- 2) wordTokenList
-- > wordTokenList ["the man is the man. he is great","the man saw the saw"]
-- ["the","man","is","the","man",".","he","is","great","the","man","saw","the","saw"]
wordTokenList :: [String] -> [String]
wordTokenList [x] = wordToken x
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs

-- 3) uniqueBigrams 
-- > uniqueBigrams ["the","man","is","the","man","."]
-- [("man","is"),("is","the"),("the","man"),("man",".")]
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) | x `elem` xs = removeDuplicates xs
                		| otherwise = x : removeDuplicates xs

uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams [] = []
uniqueBigrams (x:xs) = removeDuplicates(zip [x] xs ++ uniqueBigrams xs)

-- 4) uniqueTrigrams
-- > uniqueTrigrams ["the","man","is","the","man","."]
-- [("the","man","is"),("man","is","the"),("is","the","man"),("the","man",".")]
uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams [] = []
uniqueTrigrams list = removeDuplicates(zip3 list (drop 1 list) (drop 2 list))

-- 5) bigramsFreq
-- > bigramsFreq ["the","man","is","the","man","."]
-- [(("man","is"),1),(("is","the"),1),(("the","man"),2),(("man","."),1)]
numTimesFound :: Ord a => a -> [a] -> Integer
numTimesFound _ [] = 0
numTimesFound x list = sum $ map (\a -> 1) $ filter (== x) list

numTimesFoundList :: Ord a => [a] -> [a] -> [(a,Integer)]
numTimesFoundList [] _ = []
numTimesFoundList (x:xs) list = [(x, numTimesFound x list)] ++ numTimesFoundList xs list

uniqueBigramsWithDuplicates :: [String] -> [(String,String)]
uniqueBigramsWithDuplicates [] = []
uniqueBigramsWithDuplicates (x:xs) = zip [x] xs ++ uniqueBigramsWithDuplicates xs

bigramsFreq :: [String] -> [((String,String), Integer)]
bigramsFreq [] = []
bigramsFreq list = numTimesFoundList (uniqueBigrams list) (uniqueBigramsWithDuplicates list)

-- 6) trigramsFreq
-- > trigramsFreq ["the","man","is","the","man","."]
-- [(("the","man","is"),1),(("man","is","the"),1),(("is","the","man"),1),(("the","man","."),1)]
uniqueTrigramsWithDuplicates :: [String] -> [(String,String,String)]
uniqueTrigramsWithDuplicates [] = []
uniqueTrigramsWithDuplicates list = zip3 list (drop 1 list) (drop 2 list)

trigramsFreq :: [String] -> [((String,String,String), Integer)]
trigramsFreq [] = []
trigramsFreq list = numTimesFoundList (uniqueTrigrams list) (uniqueTrigramsWithDuplicates list)

-- 7) getFreq
-- > getFreq 'a' [('f',1),('a',2),('b',1)]
-- 2
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq _ [] = 0
getFreq char ((c, n):xs) | char == c = n
						 | otherwise = getFreq char xs

-- 8) generateOneProb
-- > generateOneProb (("the","man","is"),1) [(("he","is"),1),(("is","great"),1),(("great","the"),1),(("the","man"),3)]
-- 0.333333333333333
generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((w1, w2, w3), o) list = o / getFreq (w1, w2) list 

-- 9) genProbPairs
-- > genProbPairs [(("the","man","is"),1),(("man","is","the"),1),(("is","the","man"),1),(("the","man","."),1),(("man",".","the"),1),((".","the","man"),1),(("the","man","saw"),1)] [(("man","is"),1),(("is","the"),1),(("man","."),1),((".","the"),1),(("the","man"),3),(("man","saw"),1)]
-- [(("the","man","is"),0.333333333333333),(("man","is","the"),1.0),(("is","the","man"),1.0),(("the","man","."),0.333333333333333),(("man",".","the"),1.0),((".","the","man"),1.0),(("the","man","saw"),0.333333333333333)]
-- genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
-- genProbPairs [] _ = []
-- genProbPairs (((w1, w2, w3), o):xs) list = [((w1, w2, w3), generateOneProb ((w1, w2, w3), o) list)] ++ genProbPairs xs list

genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] list = [] 
genProbPairs (((a,b,c),r):xs) list = [((a,b,c), generateOneProb ((a,b,c),r) list)] ++ genProbPairs xs list

-- 10) generateNextWord
-- > generateNextWord ["the","man"] [(("the","man","is"),0.333333333333333),(("man","is","the"),1.0),(("is","the","man"),1.0),(("the","man","."),0.333333333333333),(("man",".","the"),1.0),((".","the","man"),1.0),(("the","man","saw"),0.333333333333333)]
-- "saw"
generateNextWord :: (Ord a, Fractional a) => [String] -> [((String,String,String),a)] -> String
generateNextWord [x, y] list = if (getPossibilitiesList (x, y) list == []) 
							   then "Sorry, it is not possible to infer from current database"
							   else getPossibilitiesList (x, y) list !! (randomZeroToX (length (getPossibilitiesList (x, y) list) - 1))

getPossibilitiesList _ [] = []
getPossibilitiesList (x, y) (((w1, w2, w3), o):xs) = if (x == w1 && y == w2 && o > 0.03 && not (w3!!0 `elem` punct)) 
												then [w3] ++ getPossibilitiesList (x, y) xs
												else  getPossibilitiesList (x, y) xs