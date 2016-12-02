
data Tree t = Branch { leftChild :: Tree t, value :: t, rightChild :: Tree t} | Leaf { value :: t } deriving (Show, Eq)


listToTree (c:cs)
			| c == '*' = Branch { leftChild = listToTree cs, value = c, rightChild = listToTree $ tail cs}
			| otherwise = Leaf { value = c }
			
mTree = listToTree ['*', '*', 'B', '*', '*', 'D', 'C', 'E', 'A']