# Monads as Map Lookups

The next two chapters -- Monads and Map Lookups, and Monads as IO -- are going to serve as motivating examples for what additional tools we need. This chapter looks at Maybe as a Monad, with containers (Maps) in the background. Then we look at IO as a Monad that explicitly encodes effects. After giving these examples as motivations, the next chapter will define a Monad in terms of the operations it supports, and without having to mention _effects_ or _Maybe_, as these are not essential elements of a Monad.

The nice thing about starting with `Maybe` as a Monad is that we can still deal with pure code, and not tie the idea that Monads are necessarily tied to effects. The downside is that it is possible, using pattern matching or case statements, to get around the need for Monads in this case: while the code is uglier, you can always "extract" the `a` out of `Just a`, and then deal with `Nothing` separately.

The `IO` Monad forces us to have to operate within the `IO` context, as we don't have a way of extracting the `String` out of an `IO String`. I put this as a separate motivating example, because `IO` requires introducing a new set of functions that we haven't used yet, and I wanted to reduce confusion by first giving an example of a Monad using only familiar functions.

## Example: Chaining two maps

Suppose we had two maps:

* `zipToCity :: M.Map Int String`. Given a zip code `Int`, return the city name `String`.
* `cityToState :: M.Map String String`. Given a city name, return the state.

This is not a great representation for this problem (in particular, the same city name can appear in many states, such as Springfield which exists in 34 states). We are going to assume that all our city names are unique, so this problem does not occur, so that we can make an example with minimal explaination.

How would we write a function that, given a zip code, would tell you the State?

Here is one work working example of the code:
```haskell
{{< include monad_map01.hs >}}
```

This works, but we can generalize this a little bit:
* A Map lookup is `lookup :: a -> M.Map a b -> Maybe b`, that is we are taking ordinary values outside of a `Maybe` context, and getting a result in a `Maybe` context.
* When chaining lookups, it is difficult because we need the original value (_not_ in a `Maybe`). We can do the deconstruction above, but it is a lot of noise
* When we get a `Nothing`, we usually want to pass that through to the end. It doesn't add a lot of value for us to pass the `Nothing` explicitly at each chained lookup.

Another way of phrasing this problem is
```haskell
lookupCityFromZip :: Int -> Maybe String
lookupCityFromZip zipCode = M.lookup zipCode zipToCity

lookupStateFromCity :: String -> Maybe String
lookupStateFromCity cityName = M.lookup cityName cityToState

-- What we want
-- useZipCodeFindState :: Int -> Maybe String
```

## Attempts to build from `fmap` or `<*>`

We have
```haskell
-- From applicative:
pure ::        a       -> Maybe a
<$>  ::       (a -> b) -> Maybe a -> Maybe b
<*>  :: Maybe (a -> b) -> Maybe a -> Maybe b

-- Functions that exist
lookupCityFromZip :: Int -> Maybe String
lookupStateFromCity :: String -> Maybe String

-- Want
useZipCodeFindState :: Int -> Maybe String
```

What we need is something that can take the result of the first lookup (`Maybe String`), and the lookup function `(String -> Maybe String)`, and give us back the result `Maybe String`.

We can try `<$>` with `a = String` and `b = Maybe String`. Then we have  `lookupStateFromCity` as a function from `a -> b`. We feed it a type `Maybe a`. Unfortunately, we get back a type `Maybe b = Maybe(Maybe String)`

```haskell
ghci> test zipCode = lookupStateFromCity <$> (lookupCityFromZip zipCode)
ghci> test 95616
Just (Just "CA")
ghci> :t test
test :: Int -> Maybe (Maybe String)
```
No combination will actually do it. We need to _either_ 

* be able to collapse sequential `Maybe` contexts (i.e. make a `Maybe(Maybe String)` into a `Maybe String`)
* or have a fucntion with the signature `(a -> Maybe b) -> Maybe a -> Maybe b`

It turns out that these will be two equivalent ways of solving the problem! The first one is called `join`, and the second is called (reverse) `bind`. You can express `join` in terms of `bind`, or `bind` in terms of `join`, so we will pick one and move forward with it.


We pick `reverse bind`:
```haskell
=<<   :: (a -> Maybe b) -> Maybe a -> Maybe b
```

Now we can contruct our chain of lookups:
```haskell
ghci> lookupCityFromZip           --  Int -> Maybe String 
ghci> lookupCityFromZip 95616     -- Maybe String
-- Lets take a = String, b = String. Then =<< 
--     a (String -> Maybe String) function, lookupStateFromCity
--     a Maybe String value, lookupCityFromZip 95616
--     returns a Maybe String value
ghci> (=<<) (lookupStateFromCity) (lookupCityFromZip 95616) -- Maybe String
-- equivalent as an infix operator
ghci> (lookupStateFromCity) =<< (lookupCityFromZip 95616) 
-- i.e. useZipCodeFindState zipCode = lookupStateFromCity =<< (lookupCityFromZip zipCode)
```

## Reverse bind `=<<` and bind `>>=`

Extending beyond `Maybe`, we have a pair of very similar functions:
```haskell
=<< :: (a -> Context b) -> Context a -> Context b  -- reverse bind
>>= :: Context a -> (a -> Context b) -> Context b  -- bind
```
These functions are not meaningfully different, we have `f =<< x` and `x >>= f` as functionally the same thing.

Reverse bind makes more sense when comparing to `<*>` and `<$>`, and looks more similar to function application (first give the function, then the value in the context being acted on). Haskell programmers tend to use `bind` or `>>=`, which takes the value in context and then pushes it through the function. 

When I was looking at bind, it seemed somewhat unnatural to do `>>=`, as you start with the value in the context, and then try to push it through a function. This is the complete opposite of what we do with functors and applicatives! However, often when starting to learn Haskell, we often have a value like an `IO String`, or a `Maybe Bool`, and the temptation is to ask "how do I get the value out of the context?". Instead, we should operate on the context. Because this is such a frequent sticking point, most Haskell teaching resources prefer bind `>>=` as you start with the troublesome object and _then_ apply a function to it.

You could write our function `useZipCodeFindState` in either of the following ways:
```haskell
-- Using reverse bind =<<.
-- Looks like regular function application:
--      start with (lookupCityFromZip zipCode) and get an output
--      take what is "inside the context" as the argument to lookupStateFromCity
--      just like g $ f x is g( f(x) )
useZipCodeFindState zipCode = lookupStateFromCity =<< (lookupCityFromZip zipCode)

-- Using bind >>= 
-- thought of as a function is weird, but it works well thinking about Unix pipes
-- 1. Calculate (lookupCityFromZip zipCode)
-- 2. Take the result from "inside the context" as the argument to lookupStateFromCity
-- 3. Calculate
-- The arrows are a visual reminder of which way we are pushing the data.
useZipCodeFindState zipCode = (lookupCityFromZip zipCode) >>= lookupStateFromCity
```

## Join and Bind

In our motivating example, we saw that we could _almost_ get away with `fmap`, except that we accumulated contexts. Specifically, we had
```haskell
ghci> test zipCode = lookupStateFromCity <$> (lookupCityFromZip zipCode)
gchi> :t test
test :: Int -> Maybe (Maybe String)
```
where what we really wanted was a function from `Int -> Maybe String`.

Let's generalize this. We have two functions, and they each take values (not in a Context), but outputs that are in a Context, and we want to chain them together:
```haskell
f1 :: a -> Context b  -- e.g. lookupCityFromZip :: Int -> Maybe String
f2 :: b -> Context c  -- e.g. lookupStateFromCity :: String -> Maybe String

-- want
chained :: a -> Context c

-- can make
doubleContext :: a -> Context(Context c)
doubleContext aType = f2 <$> f1 aType
```
Let's walk through the types:

* `f1 aType`: is of type `Context b`
* `fmap f2 (f1 aType)` has to take inputs `(X->Y)` and `Context X`, and has an output `Context Y`.
  * Looking at `f2 :: b -> Context c` this means `X = b` and `Y = Context c`.
  * So this has an output `Context Y = Context (Context c)`.

If it were possible to flatten the `Context`, we would have what we want! There is a function that does exactly this, called `join`:
```haskell
join :: Context (Context a) -> Context a
```

Let's assume that we can write `join` as a function. Then we have two ways of combining `f1` and `f2` to make `chained`:
```haskell
-- use fmap and then collapse double context to a single context
chained1 :: a -> Context c
chained1 aType = join $ doubleContext aType = join $ (f2 <$> f1 aType)

-- use >>=
chained2 :: a -> Context c
chained2 aType = f2 <<= (f1 aType)
```
These two approaches give the same result!

### Join and Bind: one in terms of the other

Niether `join` nor `bind` are more fundamental. Given one, you can define the other, to guarantee that the `chained1` and `chained2` approaches do the same thing.

Recall that `>>= :: Context a -> (a -> Context b) -> Context b`. We want a function `join :: Context (Context c) -> Context c`. This suggests:

* `a = Context c` and `b=c` in `>>=`, so we start with `Context (Context c)` and end with `Context c`
* We need a function `(a -> Context b) = (Context c -> Context c)`. The simplest such function is the identity.

And this turns out to be the definition of `join`:
```haskell
join :: Context (Context C) -> Context C
join cca = cca >>= id
```

Let's say that we had a `join` function. Can we make `bind`? Yes we can! From our example above
```haskell
chained1 :: a -> Context c
chained1 aType = join $ (f2 <$> f1 aType)

chained2 :: a -> Context c
chained2 aType = (>>=) (f1 aType) f2
```
Because these are the same, we have
```haskell
(>>=) (f1 aType) f2 = join $ (f2 <$> f1 aType)

-- as a definition
(>>=) :: Context a -> (a -> Context b) -> Context b
(>>=) ca fa_to_cb = join $ (fa_to_cb <$> ca)
```

## Maybe implementation of join and bind

Monad implementation needs us to implement `(>>=)`. The compiler could have made `join` more fundamental, but didn't.
```haskell
instance Monad MyMonad where
  (>>=) = ...
```
Let's do this for `Maybe`
```haskell
-- Note this won't run, as it conflicts with the existing declaration in the compiler
instance Monad Maybe where
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>=) Nothing _ = Nothing
    (>>=) (Just a) f = f a
```
What would join be? Let's write it manually, and then also in terms of `bind`
```haskell
-- these will run!
join :: Monad m => m (m a) -> m a
join mma = mma >>= id

join' :: Maybe (Maybe a) -> Maybe a
join' mma = case mma of 
    Nothing -> Nothing
    Just ma -> ma
```

## References

The approach of introducing Monads via sequential map lookups was inspired by _Get Programming With Haskell_ by William Kurt.

The safediv was inspired by Graham Hutton's introduction to Monads.

