mList = ['*', '*', 'B', '*', '*', 'D', 'E', 'C', 'A']

data Tree a = Branch { leftChild :: Tree a, value :: a, rightChild :: Tree a } | Leaf { value :: a } deriving (Show)

listToTree (c:cs)
	| c == '*' = (Branch { leftChild = fst leftSubTree, value = c, rightChild = fst rightSubTree }, snd rightSubTree)
	| otherwise = (Leaf { value = c }, cs)
	where	
		leftSubTree = listToTree cs
		rightSubTree = listToTree $ snd leftSubTree

mTree = fst $ listToTree mList

mEncoding = [1,0,1,1,0,1,0,1]

preorderTraverse (Leaf a) [] = (a, [])
preorderTraverse (Leaf a) (x:xs) = (a, x:xs)
preorderTraverse (Branch l v r) (x:xs)
	| x == 0 = preorderTraverse l xs
	| otherwise = preorderTraverse r xs

decode (cs) [] = reverse cs
decode (cs) (x:xs) = decode (mv:cs) (snd ml)
	where 
		ml = preorderTraverse mTree (x:xs)
		mv = fst ml


--stringToList s = concat ["[", intersperse ',' s, "]"]
--treeList = stringToList $ head . lines


main = interact getMahSheiteAndPukeItOut

getMahSheiteAndPukeItOut input = unlines . map (decode "") decodingLists
    where decodingLists = tail (lines input)
          mTree = fst $ listToTree (head $ lines input)

{-

listToTree Nil (c:cs) = listToTree (Branch Nil c Nil) cs
listToTree (Branch Nil a Nil) (c:cs)
	| c == '*' = Branch { leftChild = listToTree Nil (c:cs), value = a, rightChild = Nil }
	| otherwise = listToTree (Branch { leftChild = Leaf c, value = a, rightChild = Nil }) (cs)
listToTree (Branch l a Nil) (c:cs)
	| c == '*' = Branch { leftChild = l, value = a, rightChild = listToTree Nil (c:cs) }
	| otherwise = (Branch { leftChild = l, value = a, rightChild = Leaf c }, cs)

-}


{-
mList = ['*', '*', 'B', '*', '*', 'D', 'C', 'E', 'A']

data Tree a = Branch { leftChild :: Tree a, value :: a, rightChild :: Tree a } | Leaf { value :: a } | Nil deriving (Show)
	

listToTree cs = foldl insert Nil cs
-}

{-

listToTree Nil (c:cs) = listToTree (Branch Nil c Nil) cs
listToTree (Branch Nil a Nil) (c:cs)
	| c == '*' = Branch { leftChild = listToTree Nil (c:cs), value = a, rightChild = Nil }
	| otherwise = listToTree (Branch { leftChild = Leaf c, value = a, rightChild = Nil }) (cs)
listToTree (Branch l a Nil) (c:cs)
	| c == '*' = Branch { leftChild = l, value = a, rightChild = listToTree Nil (c:cs) }
	| otherwise = Branch { leftChild = l, value = a, rightChild = Leaf c }
	
-}
