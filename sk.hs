module Compression where

import Data.List
import Data.Char

data HTree a = Leaf Int a | Node Int (HTree a) (HTree a)
               deriving (Show)

instance Eq (HTree a) where
  t1 == t2 = freqCount t1 == freqCount t2

instance Ord (HTree a) where
  t1 <= t2' = freqCount t1 <= freqCount t2'

type Code = [Int]

freqCount :: HTree a -> Int
freqCount (Leaf n a)
  = n
freqCount (Node n t1 t2)
  = n

testString :: String
testString
  = "mississippi is missing"

--
-- Example Huffman coding tree from the spec.
--
fig :: HTree Char
fig
  = Node 22 (Node 8 (Node 4 (Leaf 2 'p') (Leaf 2 ' '))
                    (Node 4 (Node 2 (Leaf 1 'n') (Leaf 1 'g'))
                            (Leaf 2 'm')))
            (Node 14 (Leaf 7 'i') (Leaf 7 's'))

----------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Int
count x list = length (filter (== x) list)

countAll :: Eq a => [a] -> [a] -> [(a, Int)]
countAll listA listB = [(x,count x listB) | x <- listA']
  where
    listA' = nub listA

buildTable :: Eq a => [a] -> [(a, Int)]
buildTable list = countAll (nub list) list

merge :: HTree a -> HTree a -> HTree a
merge tree1 tree2 = Node n tree1 tree2
  where
    n = freqCount tree1 + freqCount tree2

reduce :: [HTree a] -> HTree a
-- Pre: The argument list non-empty and sorted based on the ordering function
--      in the Ord instance above.
reduce [t] = t
reduce (t1:t2:ts) = reduce (insert (merge t1 t2) ts)

buildTree :: Eq a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree str = reduce leaves
  where
    table = buildTable str
    leaves = genLeaves table
    genLeaves :: Eq a => [(a,Int)] -> [HTree a]
    genLeaves [] = []
    genLeaves ((content,count) : remain) = Leaf count content : genLeaves remain

flatten :: Eq a => HTree a -> a -> [Int] -> [Int]
flatten (Leaf n a) target acc
  | a == target = acc
  | otherwise = []
flatten (Node n left right) target acc = (flatten left target (0:acc)) ++ (flatten right target (1:acc))
encode :: Eq a => [a] -> HTree a -> Code
-- Pre: The tree can encode each of the items the list
encode [] any = []
encode (x:xs) tree = reverse (flatten tree x []) ++ (encode xs tree)

decode :: Code -> HTree a -> [a]
-- Pre: The code is valid with respect to the tree
decode code tree = decode' code tree tree
    where
      decode' :: Code -> HTree a -> HTree a -> [a]

      decode' list (Leaf n a) tree = [a] ++ decode' list tree tree
      decode' (x:xs) (Node n left right) tree
        | x == 0 = decode' xs left tree
        | x == 1 = decode' xs right tree
      decode' [] _ _ = []

compressTree :: HTree Char -> [Int]
compressTree
  = undefined

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree
  = undefined

-----------------------------------------------------------------------------
-- THE SECRET TEST STRING

secretString :: String
secretString
  = decode code (rebuildTree tree)
  where
    code = [0,1,0,0,0,0,1,1,0,1,0,1,1,1,1,0,0,0,1,
            1,0,0,1,1,1,1,0,0]
    tree = [0,0,0,1,1,1,0,1,0,0,0,1,1,1,0,0,0,0,1,
            0,1,1,1,0,1,1,1,1,1,1,1,1,1,0,0,1,0,1,
            1,1,0,0,0,1,0,0,0,1,0,1,0,0,0,0,1,0,1,
            0,1,0,1,1,0,0,1,1,1,1,0,0,1,1,1,0,1,0,
            0,0,0,0]
