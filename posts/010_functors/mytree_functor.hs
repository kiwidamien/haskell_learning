data Node a = Node a (Node a) (Node a) | Leaf a | Empty deriving (Eq, Show)

instance Functor Node where
    fmap :: (a -> b) -> Node a -> Node b
    fmap _ (Empty) = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a b c) = Node (f a) (fmap f b) (fmap f c)

exampleBinaryTree = Node 30 (Node 20 (Leaf 16) (Leaf 25)) (Node 50 (Node 40 (Leaf 35) (Empty)) (Node 60 Empty Empty))

