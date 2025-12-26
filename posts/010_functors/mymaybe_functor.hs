data MyMaybe a = MyJust a | MyNothing deriving (Eq, Show)

instance Functor MyMaybe where
    fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
    fmap f (MyJust a) = MyJust (f a)
    fmap f (MyNothing) = MyNothing