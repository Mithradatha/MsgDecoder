
data Tree = Branch { leftChild :: Tree, value :: Char, rightChild :: Tree } | Leaf { value :: Char } deriving (Show)

listToTree :: [Char] -> (Tree, [Char])
listToTree (c:cs)
	| c == '*' = (Branch { leftChild = fst leftSubTree, value = c, rightChild = fst rightSubTree }, snd rightSubTree)
	| otherwise = (Leaf { value = c }, cs)
	where	
		leftSubTree = listToTree cs
		rightSubTree = listToTree $ snd leftSubTree

preorderTraverse :: Tree -> [Char] -> (Char, [Char])
preorderTraverse (Leaf v) [] = (v, [])
preorderTraverse (Leaf v) (cs) = (v, cs)
preorderTraverse (Branch l v r) (c:cs)
	| c == '0' = preorderTraverse l cs
	| otherwise = preorderTraverse r cs

decode :: Tree -> [Char] -> [Char] -> [Char]
decode t (cs) [] = reverse cs
decode t (cs) (cs') = decode t (decoded:cs) (snd traversed)
	where
		traversed = preorderTraverse t (cs')
		decoded = fst traversed

split :: [[Char]] -> [[Char]]		
split (s:ss) = [ decode mTree "" s' | s' <- ss]
	where mTree = fst $ listToTree s

main :: IO ()
main = interact (unlines . split . lines)
