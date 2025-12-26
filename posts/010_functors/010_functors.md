# Functors
A functor is a way of mapping values that are in a context, while preserving the context. While this can be used for any context, we will start by looking at some container types to gain intuition.


```haskell
class Functor context  where 
    fmap :: (a -> b) -> context a -> context b
    (<$) :: a -> context b -> context a
```

That is, if we have a way of transforming `a` to `b`, and a `context` that is also a functor, then we can take an `a` or `a`s living into a context and transform them into `b` or `b`s living in the same context.


Before giving the laws of Functors, let's give two examples of common Functors: Lists and Maybe. The functors (i.e. `fmap`) is already predefined for these structures, so we can use it and think about what `fmap` is trying to do before seeing how we would implement it.

The canonical example is a list. Let's say we have a list of strings, and a function `length :: String -> Int` that takes a single string, and gives the length of that string. In this case, `fmap` is just `map`:
```haskell
ghci> map length ["cat", "dog", "rabbit"]
[3,3,6]
ghci> fmap length ["cat", "dog", "rabbit"]
[3,3,6]
```
The list in this example is a context for storing multiple strings. Let's refer to this context as a structure. We went through the "structure" `["cat", "dog", "rabbit"]` one element at a time, and applied our function `length` to each element, and then gave the output back in the same "structure". For a list, the `fmap` implementation literally is `map`!

The second example is for the structure `Maybe`. Let's say we have a `Maybe Int`, and we want to add five to it. We can do this with a function
```haskell
addFive maybeNum = case maybeNum of
    Just x -> x + 5
    Nothing -> Nothing
```
but having to make a different function if we wanted to add 6 is a pain and verbose. Instead we can use `fmap` for the `Maybe` structure:
```haskell
ghci> fmap (+5) (Just 6)
Just 11
ghci> fmap (+5) (Nothing)
Nothing
```

## Implementing a Functor and Functor Laws

Given a Abstract Data Type `structure a` (that is, a structure like a List, Maybe, Tree, Either, IO, etc) that wraps elements of type `a`, a functor maps over the values contained insiide the structure and applies a function to them, returning the wrapped values.

Let's make an artificial Maybe, and then make it a functor
```haskell
data MyMaybe a = MyJust a | MyNothing deriving (Eq, Show)

instance Functor MyMaybe where
    fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
    fmap f (MyJust a) = MyJust (f a)
    fmap f (MyNothing) = MyNothing
```

That is, we can extend any of our custom built types to get functor behavior. We now get the same behavior as we did with `Maybe`:
```haskell
ghci> :l mymaybe_functor
ghci> fmap (+5) (MyJust 6)
MyJust 11
ghci> fmap (+5) (MyNothing)
MyNothing
```

There are also two functor laws that we have to abide by. These are not enforced by the language, but by convention. Code written using `fmap` assumes that these two conditions hold:

* The law of identity: `fmap id == id`
* The law of composition: `fmap (f.g) = (fmap f) . (fmap g)`


## Implementation for a Tree

Let's suppose we made a Tree class:
```haskell
data Node a = Node a (Node a) (Node a) | Leaf a | Empty deriving (Eq, Show)
```

We can build an explicit binary tree
```haskell
exampleBinaryTree = Node 30 (Node 20 (Leaf 16) (Leaf 25)) (Node 50 (Node 40 (Leaf 35) (Empty)) (Node 60 Empty Empty))
```

How would we produce a new tree, with the same skeleton, but each value doubled? We could write a function to do that, but let's implement `fmap` for the tree.

```haskell
instance Functor Node where
    fmap :: (a -> b) -> Node a -> Node b
    fmap _ (Empty) = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a b c) = Node (f a) (fmap f b) (fmap f c)
```

Now you can write 
```haskell
ghci> :l mytree_functor
ghci> exampleBinaryTree
Node 30 (Node 20 (Leaf 16) (Leaf 25)) (Node 50 (Node 40 (Leaf 35) Empty) (Node 60 Empty Empty))
ghci> fmap (*2) exampleBinaryTree
Node 60 (Node 40 (Leaf 32) (Leaf 50)) (Node 100 (Node 80 (Leaf 70) Empty) (Node 120 Empty Empty))
```

## What did this get us?

For a container type (e.g. List, Map, our implemented Tree), `fmap` is a generalized version of `map` for lists. We go through the container, one element at a time, and have the same shape and type of container at the end. 

Our functions that map the elements don't need to know what type of data structure we are going to apply them over; instead by defining the data structure as a functor the data structure knows how to map over all the values.

`fmap` is used more generally than just containers. For example, `Maybe`, `Either`, `IO` are all contexts that hold one or zero values. We know that we cannot think about extracting the value from the context; if we make something a functor it allows us to "push" functions _into_ the context and apply them.

The `IO` context is one that causes trouble, as at the beginning we see `IO String` and want to know how to get the `String` out of `IO String`. You can't (or at least, you shouldn't). But we can use `fmap` to perform operations on `IO String` if we have functions that work on `String` types:

```haskell
ghci> x = readFile "mytree_functor.hs"
-- x is type IO String
gchi> length x
-- error: no function length::IO String -> ......
ghci> fmap length x
385   
-- length :: String -> Int, or more generally any foldable to Int,
-- so (fmap length x) has type IO Int
-- we didn't extract the string from the IO Context, we "pushed" length
-- into the context to operate on the value inside. 
ghci> length <$> x  -- <$> is the infix version of fmap
385
ghci> y = fmap length x
ghci> :i y
y :: IO Int
```

We also gain the ability to write a function once that doesn't care what type of "functor" context it will later get mapped on to, including ones that have not been written yet!

## Syntatic sugar: <$> as fmap infix

As shown in the example above, we can write `fmap` as an infix operator:
```haskell
-- option 1
fmap f x
-- option 2
f <$> x
```
These both mean the same thing.

## The "lifting" metaphor

So far we have talked about `fmap f (X a)` as "pushing" the function `f` into the context described by `X`, and operating on the `a` elements in the context.

An alterative way of thinking about `fmap` is, given a function `f:: a -> b`, then the partial application `g = fmap f` is a function `g:: X a -> X b`. That is, `fmap` transforms a function between the values and transforms it into a function that maps values in a context to values in another context. 

This is referred to "lifting" in the Haskell community. The function `f` operated between values, but the function `g = fmap f` has been "lifted" into the context `X`. It now maps between values in a context.

## Summary

* `fmap` is a generalization of what `map` was to `list` types.
  * `map f (x:xs)`: take a list as input, apply `f` to each element, and get a list as output.
  * `fmap f (x:xs)`: the same, on lists `fmap = map`
  * For a _container_ context, you generally get the same idea: you get a new container with the same "shape", but each element transformed by `f`.
* `fmap` is not only for containers! It works for contexts that cannot be thought of as containers. Intuitively, it "pushes" the value `f` into the context, but you get a result with the same context.
* Examples for built-in contexts. Let's use `even :: Int -> Bool` in each example
  * `fmap even [1,2,3] = [False, True, False]`: start with a list, end with a list
  * `fmap even (Just 3) = Just False`: Start with a Maybe, end with a Maybe
  * `fmap even (Nothing) = Nothing`: Start with a Maybe, end with a Maybe
  * `fmap even (Left 2) = Left 2`: Start with an Either, end with an Either
  * `fmap even (Right 2) = Right True`: Start with an Either, end with an Either
  * `fmap even (return 2::IO Int) = True`: The right side is `True::IO Bool`
* When writing a functor class, you should make sure that the identity function `id` does nothing when fmapped: `fmap id = id`.
* You should also make sure that your fmap implementation respects function composition:
  * `fmap (f.g) x = (fmap f) (fmap g x)`
  * In infix notation: `(f.g) <$> x = f <$> g <$> x`

To implement a functor on your own data type `X`, use
```haskell
instance Functor X where
    fmap :: (a -> b) -> X a -> X b
    ....
```

As a concrete example, here is the functor implementation on a `Node` datatype for a binary tree

```haskell
data Node a = Node a (Node a) (Node a) | Leaf a | Empty deriving (Eq, Show)

instance Functor Node where
    fmap :: (a -> b) -> Node a -> Node b
    fmap _ (Empty) = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node a b c) = Node (f a) (fmap f b) (fmap f c)
```