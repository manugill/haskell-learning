data Tree
    = Leaf Char
    | Node Tree
           Int
           Tree
    deriving (Show)

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

main = do
    putStrLn "Sup"
    print ""
    print tree
