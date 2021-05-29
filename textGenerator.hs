import DataFile

wordToken :: [Char] -> [[Char]]
wordToken l = split (words l)

split [] =[]
split (x:xs) =  if(get2 x == "") then x:split xs
                else (removeFromList(get2 x) (x)):get2 x: split xs
get2 [] =[]
get2 (x:xs) = if (elem x punct) then [x]
		      else get2 xs
removeFromList _ [] = []
removeFromList [x] (y:ys) = if (x==y) then removeFromList [x] ys
						    else y:removeFromList [x] ys
--------------------------------------------------------------------------------
wordTokenList :: [String] -> [String]
wordTokenList [] =[]
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs
--------------------------------------------------------------------------------
uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams [] =[]
uniqueBigrams (x:[]) = []
uniqueBigrams (x:y:xs) = unique((x,y):uniqueBigrams (y:xs))

unique [] =[]
unique (x:xs) | elem x xs = unique xs
			  | otherwise = x:unique xs
---------------------------------------------------------------------------------
uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams [] =[]
uniqueTrigrams (x:y:[]) = []
uniqueTrigrams (x:y:z:xs) = unique((x,y,z):uniqueTrigrams (y:z:xs))
---------------------------------------------------------------------------------
bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq l = pairLists (d) (listOfOccur (d) (nonUni l))
				where d = uniqueBigrams l

nonUni [] =[]
nonUni (x:[]) =[]
nonUni (x:y:xs) = (x,y):nonUni (y:xs)

numOfOccurences  [] _ =0
numOfOccurences  (x:xs) elem | (x == elem) = 1 + numOfOccurences xs elem
							 | otherwise = numOfOccurences xs elem

listOfOccur (x:xs) big = map (numOfOccurences big) (x:xs)

pairLists [] [] =[]
pairLists (x:xs) (y:ys) = (x,y):pairLists xs ys
----------------------------------------------------------------------------------
trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq l = pairLists (d) (listOfOccur (d) (nonUni2 l))
				 where d = uniqueTrigrams l

nonUni2 [] =[]
nonUni2 (x:y:[]) = []
nonUni2 (x:y:z:xs) = (x,y,z):nonUni2 (y:z:xs)
----------------------------------------------------------------------------------
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq _ [] =0
getFreq a ((x,y):xs) = if (a == x) then y
					   else getFreq a xs
----------------------------------------------------------------------------------
generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((a,b,c),f) l = if (getFreq(a,b) l)/=0 then (f/getFreq(a,b) l)
												  else 0
----------------------------------------------------------------------------------
genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] _ = []
genProbPairs (((x,y,z),f):xs) l = ((x,y,z),generateOneProb (((x,y,z),f)) l):genProbPairs xs l
----------------------------------------------------------------------------------
generateNextWord :: (Ord a, Fractional a) => [String] -> [((String,String,String),a)] -> String
generateNextWord [a,b] (((x,y,z),f):xs) = if (length (listOfPotential [a,b] (((x,y,z),f):xs))>0) then listOfPotential [a,b] (((x,y,z),f):xs) !! randomZeroToX (length (listOfPotential [a,b] (((x,y,z),f):xs))-1)
										  else error "Sorry, it is not possible to infer from current database"

listOfPotential _ [] =[]
listOfPotential [a,b] (((x,y,z),f):xs) = if (a==x && b==y && f>0.03) then z:listOfPotential [a,b] xs
									     else listOfPotential [a,b] xs
----------------------------------------------------------------------------------
generateText :: String -> Int -> String
generateText x num = if (num>0) then generateText new (num-1)
								else x
								where new = x ++ " " ++ generateNextWord (getLastTwo(wordToken x)) (genProbPairs (trigramsFreq (wordTokenList docs)) (bigramsFreq (wordTokenList docs)))
								
getLastTwo (x:y:[]) = [x,y]
getLastTwo (x:"":y:xs)= getLastTwo (x:y:xs)
getLastTwo (x:y:xs) = getLastTwo (y:xs)
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------