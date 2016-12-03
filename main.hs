
mList = ['*', '*', 'B', '*', '*', 'D', 'C', 'E', 'A']


data Tree a = Branch { leftChild :: Tree a, value :: a, rightChild :: Tree a } | Leaf { value :: a } | Nil deriving (Show)

{-
insert Nil v = Branch { leftChild = Nil, value = v, rightChild = Nil }
insert (Branch Nil a Nil) v
	| v == '*' = Branch { leftChild = (insert Nil v), value = a, rightChild = Nil }
	| otherwise = Branch { leftChild = (Leaf { value = v }), value = a, rightChild = Nil }
insert (Branch (Leaf b) a Nil) v
	| v == '*' = Branch { leftChild = (Leaf b), value = a, rightChild = (insert Nil v) }
	| otherwise = Branch { leftChild = (Leaf b), value = a, rightChild = (Leaf { value = v }) }
insert (Branch l a Nil) v
	| v == '*' = Branch { leftChild = l }
-}

listToTree x [] = x
listToTree Nil (c:cs) = listToTree (Branch Nil c Nil) cs
listToTree (Branch Nil a Nil) (c:cs)
	| c == '*' = Branch { leftChild = listToTree Nil (c:cs), value = a, rightChild = Nil }
	| otherwise = listToTree (Branch { leftChild = Leaf c, value = a, rightChild = Nil }) (cs)
listToTree (Branch l a Nil) (c:cs)
	| c == '*' = Branch { leftChild = l, value = a, rightChild = listToTree Nil (c:cs) }
	| otherwise = Branch { leftChild = l, value = a, rightChild = Leaf c }


























{-data Tree t = Branch { leftChild :: Tree t, value :: t, rightChild :: Tree t} | Leaf { value :: t } deriving (Show, Eq)


listToTree (c:cs)
			| c == '*' = Branch { leftChild = listToTree cs, value = c, rightChild = listToTree $ tail cs}
			| otherwise = Leaf { value = c }
			
			
mList = ['*', '*', 'B', '*', '*', 'D', 'C', 'E', 'A']
mTree = listToTree mList-}

{-insertTree a Empty = Branch Empty a Empty
insertTree a (Branch Empty v Empty)
			| a == '*' = Branch (insertTree a Empty) v Empty
			| otherwise = Branch (Leaf a) v Empty
insertTree a (Branch l v Empty)
			| a == '*' = Branch l v (insertTree a Empty)
			| otherwise = Branch l v (Leaf a)
insertTree a (Branch l v r) = Branch l v r

mTree = foldl insertTree Empty mList-}




