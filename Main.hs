{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Lev Dubinets
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless, forM)
import Data.List --(stripPrefix, groupBy, tails, group)
import System.Exit (exitFailure)

--For problems 22 to 2X
import System.Random 

--1 Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No last element of empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs


--2 Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "No second to last element of empty list"
myButLast (x:[]) = error "No second to last element of singleton list"
myButLast (x:y:[]) = x
myButLast (x:y:xs) = myButLast (y:xs)

--3 Find the K'th element of a list. The first element in the list is number 1.
myElementAt :: (Num b, Eq b) => [a] -> b -> a
myElementAt [] 1 = error "No such element"
myElementAt (x:_) 1 = x
myElementAt (x:xs) b = myElementAt xs (b - 1)

--4 Find the number of elements of a list.
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (x:[]) = 1
myLength (x:xs) = 1 + myLength(xs)

myLength' :: (Num b) => [a] -> b
myLength' = sum . map (\_->1)

--5 Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]

--6 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True                --base case for even length
isPalindrome (x:[]) = True            --base case for odd  length
isPalindrome (x:xs) =
    if x == last (xs)
        then isPalindrome (init xs)
        else False

--7 Flatten a nested list
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x)  = [x]
myFlatten (List xs) = concat (map myFlatten xs)
--myFlatten (List []) = []

myFlatten' :: NestedList a -> [a]
myFlatten' (Elem a   )   = [a]
myFlatten' (List (x:xs)) = myFlatten' x ++ myFlatten' (List xs)
myFlatten' (List [])     = []

--8 Compress list with dupes
compress :: (Eq a) => [a] -> [a]
compress xs = map head (group xs)

compress' :: (Eq a) => [a] -> [a]
compress' []     = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)

--9 Pack array (Similar to Data.List.group)
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let sp = span (==x) (x:xs)
    in (fst sp):(pack (snd sp))

--10 Run length encoding
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = let sp = span (==x) (x:xs)
    --in (fst sp):(pack (snd sp))
    in (length (fst sp), x):(encode (snd sp))

--11 RLE with custom type
data RunLengthEncoding a = Single a | Multiple Int a deriving (Show, Read)
encodeModified :: (Eq a) => [a] -> [RunLengthEncoding a]
encodeModified [] = []
encodeModified (x:xs) =
    if lenFst == 1
    then Single x:(encodeModified (snd sp))
    else Multiple lenFst x:(encodeModified (snd sp))
    where
        sp = span (==x) (x:xs)
        lenFst = length (fst sp)
    --in (fst sp):(pack (snd sp))
    --in (length (fst sp), (fst sp)):(encode (snd sp))

encodeModified' :: Eq a => [a] -> [RunLengthEncoding a]
encodeModified' = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x

--12 Decode RLE (with custom type)
decodeModified :: (Eq a) => [RunLengthEncoding a] -> [a]
decodeModified [] = []
decodeModified (Single a:xs) = a:decodeModified xs
decodeModified ((Multiple amt a):xs) = replicate amt a ++ decodeModified xs

decodeModified' = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

--13 Direct RLE... Not sure I did this right
--   encode 'directly' without splitting into sublists. Is using dropWhile allowed?
encodeDirect :: (Eq a) => [a] -> [RunLengthEncoding a]
encodeDirect [] = []
encodeDirect (x:xs) =
    if length xs == length droppedXs
    then Single x:encodeDirect(xs)
    else Multiple (length (x:xs) - length droppedXs) x:(encodeModified droppedXs)
    where droppedXs = dropWhile (==x) xs

--14 Duplicate elements of list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

--15 Replication elements a given num of times
repli :: [a] -> Int -> [a]
repli [] amt = []
repli (x:xs) amt = repliElem x amt ++ repli xs amt
    where
        repliElem x 0 = []
        repliElem x amt = x:repliElem x (amt - 1)

--16 Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs amt =
    if length xs < amt
    then xs
    else (fst splitList) ++ (dropEvery (tail $ snd splitList) amt)
    where splitList = splitAt (amt-1) xs

--17  Split a list into two parts; the length of the first part is given.
--split "abcdefghik" 3
--("abc", "defghik")

mySplit :: [a] -> Int -> ([a], [a])
mySplit [] _ = ([], [])
mySplit xs 0 = ([], xs)
mySplit (x:xs) i 
    | i < 0 = mySplit (x:xs) ((length (x:xs)) - (negate i))
    | i > length (x:xs) = ((x:xs), [])
    | otherwise = (taken, dropped)
        where
            theSplit = mySplit (xs) (i - 1)
            taken = x:(fst theSplit)
            dropped = snd theSplit

--18 Extract a slice from a list.
--   Given two indices, i and k, the slice is the list containing the elements between
--      the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
-- slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"

slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b - (a - 1)) dropped
    where dropped = drop (a - 1) xs

--19 Rotate a list N places to the left.
--Hint: Use the predefined functions length and (++).
--  rotate ['a','b','c','d','e','f','g','h'] 3
--  "defghabc"
--  rotate ['a','b','c','d','e','f','g','h'] (-2)
--  "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) a 
    | a > 0 = rotate (xs ++ [x]) (a-1)
    | a < 0 = rotate (x:xs) (length (x:xs) - negate a)


--20 Remove the K'th element from a list.
--Modified from original to use a Maybe
--removeAt 2 "abcd"
--('b',"acd")
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt a xs 
    | a > length xs = (Nothing, xs)
    | a < 0 = (Nothing, xs)
    | otherwise = (Just (xs !! (a - 1)), slice xs 1 (a - 1) ++ slice xs (a + 1) (length xs))

removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ [] = (Nothing, [])
removeAt' 1 (x:xs) = (Just x, xs)
removeAt' k (x:xs) = let (a, r) = removeAt' (k - 1) xs in (a, x:r)

removeAt'' n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)

--21 Insert an element at a given position into a list.
--insertAt 'X' "abcd" 2
--"aXbcd"

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1 = e:xs
insertAt e (x:xs) p = x:(insertAt e xs (p-1))

--22 Create a list containing all integers within a given range.
--range 4 9
--[4,5,6,7,8,9]
range :: (Ord a, Enum a) => a -> a -> [a]
range a b 
    | a == b = [a]
    | a > b  = a:range (pred a) b
    | a < b  = a:range (succ a) b

--Note: As I started to solve the problems that involve random numbers, and thus
--   began straying away from the pureness of haskell, the programming became 
--   less 'beautiful', perse, and more imperative and boring.

--23 Extract a given number of randomly selected elements from a list.
--rnd_select "abcdefgh" 3 >>= putStrLn
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    elems <- forM [1..n] (\a -> do 
        index <- randomRIO (0, (length xs - 1))
        return (xs !! index))
    return elems

--24 Lotto: Draw N different random numbers from the set 1..M.
--Example in Haskell:
-- diff_select 6 49
-- [23,1,17,33,21,37]
diff_select :: Int -> Int -> IO [Int]
diff_select n m = do 
    nums <- forM [1..n] (\a -> do
        num <- randomRIO (1, m)
        return num)
    return nums

--25 Generate a random permutation of the elements of a list.
--  rnd_permu "abcdef"
--  "badcef"
rnd_permu :: [a] -> IO [a]
rnd_permu [] = do
    return []
rnd_permu xs = do
    index <- randomRIO (0, (length xs - 1))
    rest <- rnd_permu $ snd (removeAt'' (index + 1) xs)
    return $ (xs !! index) : rest

--26 Generate the combinations of K distinct objects chosen from the N elements of a list
--  In how many ways can a committee of 3 be chosen from a group of 12 people?
--  We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients).
--  For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
--Wow! These 5 Lines of code really tested my Haskell abilities...
--combinations 3 "abcdef"
--["abc","abd","abe",...]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = zipWith (:) (replicate (length rest) x) rest ++ (combinations n xs)
    where rest = (combinations (n-1) xs)

--27 Group the elements of a set into disjoint subsets.
--  a) In how many ways can a group of 9 people work in 3 disjoint
--  subgroups of 2, 3 and 4 persons? Write a function that generates all
--  the possibilities and returns them in a list.
--  b) Generalize the above predicate in a way that we can specify a
--  list of group sizes and the predicate will return a list of groups.
--  Note that we do not want permutations of the group members;
--  i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).
--  However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...)
--  and ((CARLA DAVID) (ALDO BEAT) ...).
--  You may find more about this combinatorial problem in a good book
--  on discrete mathematics under the term "multinomial coefficients".
--  Example:
--  P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--  [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
--  (altogether 1260 solutions)
--  27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
--  [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
--  (altogether 756 solutions)
-- 

--helper function to see if any elem of x is contained in y
--anyElem [1,2] [3,2,4] -> True
--anyElem [1,2,3] [6,7,5,8,7] -> False
anyElem :: (Eq a) => [a] -> [a] -> Bool
anyElem _ [] = False
anyElem (x:[]) ys = x `elem` ys
anyElem (x:xs) ys = if x `elem` ys then True else anyElem xs ys

--this might be golfing, but
--  I basically generate all the groups of 2,3, and 4 workers (called subgroups soon)
--  then the list comprehension joins them in such a way that no two subgroups have
--  the same element.
group3 :: (Eq a) => [a] -> [[[a]]]
group3 elems = [[x,y,z] 
    | x <- combinations 2 elems
    , y <- filter (not . anyElem x) (combinations 3 elems)
    , z <- filter (\zs -> not $ or [(anyElem zs x),(anyElem zs y)]) (combinations 4 elems)]

--part B

--It would be cool to generate a list-comprehension-type-thing for any amt of elems where
--  I could continue filtering out elements that were previously picked. But it would be
--  wildly inefficient anyway. So instead, lets modify combinations to also return the
--  elems it didn't pick (so I know what is left to choose from in the groups).
combinationTuples :: Int -> [a] -> [([a],[a])]
combinationTuples 0 xs     = [([],xs)]
combinationTuples n []     = []
combinationTuples n (x:xs) = 
    let firstElemTuples     = [ (x:taken,dropped) | (taken,dropped) <- combinationTuples (n-1) xs ]
        dropFirstElemTuples = [ (taken,x:dropped) | (taken,dropped) <- combinationTuples  n    xs ]
        in firstElemTuples ++ dropFirstElemTuples
 
myGroup :: [Int] -> [a] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (a:amts) elems = [ x:xs | y <- combinationTuples a elems, (x,rs) <- [(fst y, snd y)], xs <- myGroup amts rs ]

--28 Sorting a list of lists according to length of sublists
--  a) We suppose that a list contains elements that are lists themselves. The objective is to sort the
--  elements of this list according to their length. E.g. short lists first, longer lists later, or vice
--   versa.
--Example in Haskell:
--Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
--Prelude>["o","de","de","mn","abc","fgh","ijkl"]

--b) Again, we suppose that a list contains elements that are lists themselves. But this time the 
--  objective is to sort the elements of this list according to their length frequency; i.e., in the
--  default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a 
--  more frequent length come later.
--Example in Haskell:
--lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
--["ijkl","o","abc","fgh","de","de","mn"]

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort [x] = [x]
lsort xs = lmerge (lsort left) (lsort right)
    where (left, right) = splitAt (div (length xs) 2) xs 

lmerge :: [[a]] -> [[a]] -> [[a]]
lmerge xs [] = xs
lmerge [] ys = ys
lmerge (x:xs) (y:ys) 
    | length x < length y = x : lmerge xs (y:ys)
    | otherwise           = y : lmerge (x:xs) ys

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort [x] = [x]
lfsort xs = concat $ lsort $ groupBy (\a b -> length a == length b) (lsort xs)


-- 31 Determine whether a given integer number is prime. 
-- isPrime 7
-- True
sieveEratosthenes :: [Int] -> [Int]
sieveEratosthenes [] = []
sieveEratosthenes (x:[]) = x:[]
sieveEratosthenes (x:xs) = 
    x : sieveEratosthenes (xs \\ [x*x, x*x + 2*x .. (last xs)])


divisibleByAnythingInList :: [Int] -> Int -> Bool
divisibleByAnythingInList divisors n = any (\d -> (mod n d) == 0 ) divisors

isPrime :: Int -> Bool
isPrime n = not $ divisibleByAnythingInList (2: sieveEratosthenes [3,5..(1 + (round $ sqrt (fromIntegral n)))]) n

--32 Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
--[9,3,3]

myGCD :: Int -> Int -> Int
myGCD a 0 = abs a
myGCD a b = myGCD b (mod a b)

--33 Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--coprime 35 64
--True

coprime :: Int -> Int -> Bool
coprime a b
    | myGCD a b == 1 = True
    | otherwise = False

-- 34 Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
-- totient 10
-- 4

totient :: Int -> Int
totient a 
    | a < 1 = error "totient is not defined for negatives"
    | otherwise = length [x | x <- [1..a], coprime a x ==True]

splitIntoSingleStrings xs = groupBy (\a b -> False) xs

-- 35 Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
-- primeFactors 315
-- [3, 3, 5, 7]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n 
    | isPrime n = [n]
    | otherwise = (head shortSieve) : primeFactors (n `div` (head shortSieve))
        where
            sieve = (2: sieveEratosthenes [3,5..(1 + (round $ sqrt (fromIntegral n)))])
            shortSieve = dropWhile (\a -> n `mod` a /= 0) sieve

--36 Determine the prime factors of a given positive integer.
--Construct a list containing the prime factors and their multiplicity.
--prime_factors_mult 315
--[(3,2),(5,1),(7,1)]

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map (\a -> (head a, length a)) groupedPrimes
    where
        groupedPrimes = group $ primeFactors n

--37 Calculate Euler's totient function phi(m) (improved).
phi :: Int -> Int
phi m = product $ map (\(p, m) -> (p - 1) * p ^ (m-1)) primesMult
    where primesMult = primeFactorsMult m

--39 Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
--  primesR 10 20
--  [11,13,17,19]
primesR :: Int -> Int -> [Int]
primesR low high = filter (> low) (2:sieveEratosthenes [3,5 .. high])

--40 Goldbach's conjecture.
--Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.
--goldbach 28
--(5, 23)

goldbach :: Int -> (Int, Int)
goldbach n = 
    let 
        primes = 2: sieveEratosthenes [3,5..n]
    in head [(x,y) | x <- primes, y <- primes, x+y == n]

--41 Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
--In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.
--Exercises> goldbachList 9 20
--[(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
--Exercises> goldbachList' 4 2000 50
--[(73,919),(61,1321),(67,1789),(61,1867)]

goldbachList low high = 
    map (\a -> goldbach a) $ filter (\a -> mod a 2 == 0) [low..high]
 
goldbachList' low high threshold = 
    filter (\(a,b) -> if a >= threshold && b >= threshold 
        then True else False) (goldbachList low high)

--46 Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed
--   or fail according to the result of their respective operations; e.g. and(A,B) will succeed, if and only if 
--   both A and B succeed.
--A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
--Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
-- table (\a b -> (and' a (or' a b)))
--True True True
--True False True
--False True False
--False False False
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

nand' :: Bool -> Bool -> Bool
nand' a b = not' $ and' a b 

not' :: Bool -> Bool
not' True = False
not' False = True

nor' :: Bool -> Bool -> Bool
nor' a b = not' $ or' a b

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _        = False

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _ _ = True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _ _ = False

table :: (Bool -> Bool -> Bool) -> [Char]
table pred =
    "True " ++ "True " ++ show (pred True True) ++ "\n" 
    ++ "True " ++ "False " ++ show (pred True False) ++ "\n" 
    ++ "False " ++ "True " ++ show (pred False True) ++ "\n" 
    ++ "False " ++ "False " ++ show (pred False False)

--47 Truth tables for logical expressions (2).
--Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical 
--  expression in the more natural way, as in the example: A and (A or not B). Define operator precedence 
--  as usual; i.e. as in Java.
--table2 (\a b -> a `and'` (a `or'` not b))
--True True True
--True False True
--False True False
--False False False

infixl 4 `or'`
infixl 6 `and'`

myList = [2,3,43,3,4,2,5,6,17]
myString = "abba"

-- Hello World
exeMain = do
    putStrLn ("myList: " ++ show (myList))
    putStrLn ("1: Find the last element of a list.")
    putStrLn ("-- " ++ show (myLast (myList)))
    putStrLn ("2: Find the last but one element of a list.")
    putStrLn ("-- " ++ show (myButLast (myList)))
    putStrLn ("3: Find the K'th element of a list. The first element in the list is number 1.")
    putStrLn ("-- " ++ show (myElementAt myList 2))
    putStrLn ("4: Find the number of elements of a list.")
    putStrLn ("-- " ++ show (myLength myList))
    putStrLn ("5: Reverse a list.")
    putStrLn ("-- " ++ show (myReverse myList))
    putStrLn ("6: Find out whether a list is a palindrome.")
    putStrLn ("-- " ++ show (isPalindrome myList))
    putStrLn ("-- " ++ show (isPalindrome "abba"))
    putStrLn ("-- " ++ show (isPalindrome "sabbath"))
    putStrLn ("7: Flatten a nested list.")
    putStrLn ("-- " ++ show (myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])))
    putStrLn ("-- " ++ show (myFlatten (Elem 5)))
    putStrLn ("-- " ++ myFlatten (List []))
    putStrLn ("8: Compress list with dupes.")
    putStrLn ("-- " ++ show (compress "aaaabccaadeeee"))
    putStrLn ("-- " ++ show (compress' "aaaabccaadeeee"))
    putStrLn ("9: Pack array (Similar to Data.List.group).")
    putStrLn ("-- " ++ show (pack "aaaabccaadeeee"))
    putStrLn ("10: Run length encoding.")
    putStrLn ("-- " ++ show (encode "aaaabccaadeeee"))
    putStrLn ("11: RLE with custom type.")
    putStrLn ("-- " ++ show (encodeModified "aaaabccaadeeee"))
    putStrLn ("-- " ++ show (encodeModified' "aaaabccaadeeee"))
    putStrLn ("12: Decode RLE (with custom type).")
    putStrLn ("-- " ++ show (decodeModified
        [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']))
    putStrLn ("-- " ++ show (decodeModified'
        [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']))
    putStrLn ("13: Direct RLE... Not sure I did this right.")
    putStrLn ("-- " ++ show (encodeDirect "aaaabccaadeeee"))
    putStrLn ("14: Duplicate elements of list.")
    putStrLn ("-- " ++ show (dupli [1,2,3]))
    putStrLn ("15: Replication elements a given num of times.")
    putStrLn ("-- " ++ show (repli [1,2,3] 2))
    putStrLn ("-- " ++ show (repli [1,2,3] 3))
    putStrLn ("16: Drop every N'th element from a list.")
    putStrLn ("-- " ++ show (dropEvery "abcdefghik" 3))
    putStrLn ("17: Split a list into two parts; the length of the first part is given.")
    putStrLn ("-- " ++ show (mySplit "abcdefghik" 3))
    putStrLn ("-- " ++ show (mySplit "abcdefghik" 0))
    putStrLn ("-- " ++ show (mySplit "abcdefghik" (-1)))
    putStrLn ("18: Extract a slice from a list.")
    putStrLn ("-- " ++ show (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7))
    putStrLn ("19: Rotate a list N places to the left.")
    putStrLn ("-- " ++ show (rotate ['a','b','c','d','e','f','g','h'] 3))
    putStrLn ("-- " ++ show (rotate ['a','b','c','d','e','f','g','h'] (-2)))
    putStrLn ("20 Remove the K'th element from a list.")
    putStrLn ("-- " ++ show (removeAt 2 "abcd"))
    putStrLn ("21 Insert an element at a given position into a list.")
    putStrLn ("-- " ++ show (insertAt 'X' "abcd" 2))
    putStrLn ("22 Create a list containing all integers within a given range.")
    putStrLn ("-- " ++ show (range 4 9))
    putStrLn ("-- " ++ show (range 9 4))
    putStrLn ("-- " ++ show (range 'a' 'z'))
    putStrLn ("23 Extract a given number of randomly selected elements from a list")
    rnd_1 <- rnd_select "abcdefgh" 3
    putStrLn ("-- " ++ show (rnd_1))
    putStr ("-- ")
    rnd_select "abc" 1 >>= putStrLn
    putStrLn ("24 Lotto: Draw N different random numbers from the set 1..M.")
    putStr ("-- ")
    diff_select 5 59 >>= putStrLn . show
    diff_1 <- diff_select 5 59
    putStrLn ("-- " ++ show (diff_1))
    putStrLn ("25 Generate a random permutation of the elements of a list.")
    putStr ("-- ")
    rnd_permu "abcdef" >>= putStrLn . show
    putStr ("-- ")
    rnd_permu "" >>= putStrLn . show
    putStrLn ("26 Generate the combinations of K distinct objects chosen from the N elements of a list.")
    putStrLn ("--" ++ show (combinations 2 [1,2,3]))
    putStrLn ("27 Group the elements of a set into disjoint subsets.")
    --putStrLn ("-- " ++ show (length (group3 ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"])))
    putStrLn ("-- My implementation of 27A is fairly slow, so I've commented out its test")
    putStrLn ("   because it is made obsolete by 27B")
    putStrLn ("27 Part B")
    putStrLn ("-- " ++ show (length (myGroup [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"])))
    putStrLn ("28 Sorting a list of lists according to length of sublists")
    putStrLn ("-- " ++ show (lsort ["abc","de","fgh","de","ijkl","mn","o"]))
    putStrLn ("28 Part B")
    putStrLn ("-- " ++ show (lfsort ["abc","de","fgh","de","ijkl","mn","o"]))
    putStrLn ("31 Determine whether a given integer number is prime.")
    putStrLn ("--" ++ show(isPrime 7))
    putStrLn ("32 Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.")
    putStrLn ("--" ++ show([myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]))
    putStrLn ("33 Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.")
    putStrLn ("--" ++ show(coprime 35 64))
    putStrLn ("34 Calculate Euler's totient function phi(m).")
    putStrLn ("--" ++ show (totient 10))
    putStrLn ("35 Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.")
    putStrLn ("--" ++ show (primeFactors 315))
    putStrLn ("36 Determine the prime factors of a given positive integer.")
    putStrLn ("   Construct a list containing the prime factors and their multiplicity")
    putStrLn ("--" ++ show (primeFactorsMult 315))
    putStrLn ("37 Calculate Euler's totient function phi(m) (improved).")
    putStrLn ("--" ++ show (phi 10))
    putStrLn ("39 Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.")
    putStrLn ("--" ++ show (primesR 10 20))
    putStrLn ("46 Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed")
    putStrLn ("--" ++ table (\a b -> (and' a (or' a b))))
    putStrLn ("47 Define operators and precedence for and' and or'")
    putStrLn ("--" ++ table (\a b -> a `and'` (a `or'` not b)))


-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION


