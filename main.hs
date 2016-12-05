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

mDecoded = decode "" mEncoding



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

insert Nil c = Branch { leftChild = Nil, value = c, rightChild = Nil }
insert (Branch Nil v Nil) c
	| c == '*' = Branch { leftChild = (insert Nil c), value = v, rightChild = Nil }
	| otherwise = Branch { leftChild = (Leaf c), value = v, rightChild = Nil }
insert (Branch (Branch Nil iv Nil) v Nil) c
	| c == '*' = Branch { leftChild = (Branch { leftChild = (insert Nil c), value = iv, rightChild = Nil }), value = v, rightChild = Nil }
	| otherwise = Branch { leftChild = (Branch { leftChild = (Leaf c), value = iv, rightChild = Nil }) value = v, rightChild = Nil }
insert (Branch (Branch l iv Nil) v Nil) c
	| c == '*' = Branch { leftChild = (Branch { leftChild = l, value = iv, rightChild = (insert Nil c) }), value = v, rightChild = Nil }
	| otherwise = Branch { leftChild = (Branch { leftChild = l, value = iv, rightChild = (Leaf c) }), value = v, rightChild = Nil }
insert (Branch l v Nil) c
	| c == '*' = Branch { leftChild = l, value = v, rightChild = (insert Nil c) }
	| otherwise = Branch { leftChild = l, value = v, rightChild = (Leaf c) }
insert (Branch l v (Branch Nil iv Nil)) c
	| c == '*' = Branch { leftChild = l, value = v, rightChild = (Branch { leftChild = (insert Nil c), value = iv, rightChild = Nil }) }
	| otherwise = Branch { leftChild = l, value = v, rightChild = (Branch { leftChild = (Leaf c), value = iv, rightChild = Nil }) }
insert (Branch l v (Branch il iv Nil)) c
	| c == '*' = Branch { leftChild = l, value = v, rightChild = (Branch { leftChild = il, value = iv, rightChild = (insert Nil c) }) }
	| otherwise = Branch { leftChild = l, value = v, rightChild = (Branch { leftChild = il, value = iv, rightChild = (Leaf c) }) }
	

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
