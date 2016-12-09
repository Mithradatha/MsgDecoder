{-
- Author: Samuel Kellar, skellar2014@my.fit.edu
- Author: Dmitry Kulakov, dkulakov2014@my.fit.edu
- Course: CSE 4250, Fall 2016
- Project: Proj3, Decoding Text
-}

data Tree = Branch { leftChild :: Tree, value :: Char, rightChild :: Tree } | Leaf { value :: Char } deriving (Show)

-- Recursively creates the preorder tree out of each element in the given list
listToTree :: [Char] -> (Tree, [Char])
listToTree [] = (Leaf { value = ' '}, [])
listToTree (c:cs)
    | c == '*' = (Branch { leftChild = fst leftSubTree, value = c, rightChild = fst rightSubTree }, snd rightSubTree)
    | otherwise = (Leaf { value = c }, cs)
    where
        leftSubTree = listToTree cs
        rightSubTree = listToTree $ snd leftSubTree
		-- Maps over the remaining elements after first pass of left sub tree

-- Traverses the tree in preorder fashion until collision with leaf
-- 0 indicates left sub tree, 1 indicates right sub tree
preorderTraverse :: Tree -> [Char] -> (Char, [Char])
preorderTraverse (Leaf v) [] = (v, [])
preorderTraverse (Leaf v) (cs) = (v, cs)
preorderTraverse (Branch _ _ _) [] = (' ', [])
preorderTraverse (Branch l _ r) (c:cs)
    | c == '0' = preorderTraverse l cs
    | otherwise = preorderTraverse r cs

-- Recursively decodes given sequence and returns decoded message
decode :: Tree -> [Char] -> [Char] -> [Char]
decode _ (cs) [] = reverse cs
decode t (cs) (cs') = decode t (decoded:cs) (snd traversed)
    where
        traversed = preorderTraverse t (cs')
        decoded = fst traversed

-- First line is tree structure, tail composed of individual messages
split :: [[Char]] -> [[Char]]
split [] = [[]]    
split (s:ss) = [ decode mTree "" s' | s' <- ss]
    where mTree = fst $ listToTree s

-- "I can't go back to yesterday - because I was a different person then."
main :: IO ()
main = interact (unlines . split . lines)
