
mList = ['*', '*', 'B', '*', '*', 'D', 'C', 'E', 'A']

data Tree a = Branch { leftChild :: Tree a, value :: a, rightChild :: Tree a } | Leaf { value :: a } | Nil deriving (Show)

listToTree Nil (c:cs) = listToTree (Branch Nil c Nil) cs
listToTree (Branch Nil a Nil) (c:cs)
	| c == '*' = Branch { leftChild = listToTree Nil (c:cs), value = a, rightChild = Nil }
	| otherwise = listToTree (Branch { leftChild = Leaf c, value = a, rightChild = Nil }) (cs)
listToTree (Branch l a Nil) (c:cs)
	| c == '*' = Branch { leftChild = l, value = a, rightChild = listToTree Nil (c:cs) }
	| otherwise = Branch { leftChild = l, value = a, rightChild = Leaf c }
