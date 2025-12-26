# Applicatives

Every Applicative is also a functor. The definition is 
```haskell
class Functor context => Applicative context where 
    pure :: a -> context a
    (<*>) :: context (a -> b) -> context a -> context b
```
This extends `Functor`, so we already have `fmap/<$>` defined for this context as well.

This is something I had a lot of trouble getting used to, so let's start with a motivating example.

## Motivational examples

### Currency converter

The problem we are going to solve is building a currency converter. 

What we would _like_ is to be enter a currency in one denomination, and get the value in another denomination, like so
```haskell
ghci> convertCurrency (Currency "YEN" 10000) "NZD"
Just Currency "NZD" 109.60 
```
Here we return a `Maybe` datatype, in case the user gives us a currency that does not exist, or that we have not tracked.

Let's start by assuming that all currency transactions are transitive, and then getting some currency exchange rates against the USD.
```haskell
{{< include converter01.hs >}}
```
Doing a manual lookup, 1 USD = 156.02 Yen = 1.71 NZD, so our convert function to answer the question "how many NZD is 10k YEN" would be
```haskell
ghci> :l converter01
ghci> convert 156.02 1.71 10000
109.60
```
We want to move from a function where you use the rates explicitly, to one where we use our lookup table.

Here is one way to do it that works
```haskell
convertCurrency :: Currency -> String -> Maybe Currency
convertCurrency (Currency nameSrc value) nameDest = case M.lookup nameSrc oneUSDTo of
    Nothing -> Nothing
    Just rateSrc -> case M.lookup nameDest oneUSDTo of
        Nothing -> Nothing
        Just rateDest -> Just (Currency nameDest (convert rateSrc rateDest value))
```

But this obscutes the actual logic of what we want. Let's write something that is _not_ valid, but gets the idea of what we want
```haskell
-- Note this doesn't work
convertCurrency :: Currency -> String -> Maybe Currency
convertCurreny (Currency nameSrc value) nameDest = Currency nameDest (convert' rateSrc rateDest value)
  where rateSrc :: Maybe Float = M.lookup nameSrc oneUSDTo
        rateDest :: Maybe Float = M.lookup nameDest oneUSDTo
        convert' :: Maybe Float -> Maybe Float -> Maybe Float 
        convert' msrc mdest value = ..... -
```
There are two problems here:

1. The returned type is a `Currency`, not a `Maybe Currency`. We need to do the work to see if the `convert'` function fails.
2. If there were no `Maybe` functors around, we would be able to write `convert'` pretty simply as `convert' rateSrc rateDest value = convert rateSrc rateDest value`. But the `rateSrc` and `rateDest` living inside a `Maybe` stops us from doing this.

When we had a function with only one argument, then `fmap` allowed us to push the function inside the container. An applicative will allow us to do this for functions with "multiple arguments" (technically all Haskell functions have one argument, but we often think of a function as taking multiple arguments).

### Binary functions: why `fmap` isn't enough

Let's start with a simpler example, with the binary function addition. We know the function 
```haskell
(+3) :: Num a => a -> a
```
is a function that takes a number to another number. If we wanted to operate on a `Maybe Int`, we can push the function into the `Maybe` as follows:
```haskell
ghci> fmap (+3) (Just 5)
Just 8
```
But this doesn't work:
```haskell
ghci>  fmap (+) (Just 3) (Just 5)
-- error
```
We would need an `fmap2` with an argument
```haskell
fmap2 :: (a->b->c) -> context a -> context b -> context c
```
If we wrote `fmap2`, then we would need a separate `fmap3` for three arguments, and so on.

Instead, Haskell uses two operations that look weird on first glance:
```haskell
class (Functor context) => Applicative context where
    pure  :: a -> context a
    (<*>) :: context (a -> b) -> context a -> context b
```


Let's start with why `<*>` is useful, and compare to `fmap`:
```haskell
  fmap :: (a -> b) -> context a -> context b
```
Let's start by trying to apply `fmap` to `+` in the following way:
```haskell
gchi> partial = fmap (+) (Just 2)
gchi> :i partial
partial :: Num a => Maybe (a -> a)
```
It might be surprising that `partial` was defined -- doesn't `fmap` only take functions of a single variable? Yes, but in Haskell all functions are a functions of a single variable! We generally think of addition as a binary operator, taking two numbers to get a number, but recall
```haskell
(+3) :: Num a => a -> a
```
Applying one argument (e.g. 3) to addition gives you a new function `(+3)`, which maps a number to the number plus three. In the `fmap` syntax, using some extra parens:
```
fmap (+) (Just 2)              =
     (a->(a->a)) (Maybe a)     = (Maybe a->a)
```
The result of "pushing" `(+)` into the `Maybe` is that we now have a function `a->a` inside a `Maybe`. Now we can see how the 
```haskell
<*>: Maybe (a->b) -> Maybe a -> Maybe b
```
can be useful. Very verbosely, we have
```haskell
ghci> (fmap (+) (Just 2)) <*> (Just 5)
Just 7
-- using the infix operator
ghci> ( (+) <$> (Just 2)) <*> (Just 5)
Just 7
ghci> (+) <$> (Just 2) <*> (Just 5)
Just 7
```

This gives us a general strategy:

1. The first `fmap` will place the partially applied function in the context.
2. We can then use `<*>` for all the remaining arguments, as we will get back partially applied functions in the context, or the final value in the context.

We will refine this strategy in a moment.

### Back to the currency converter 

Let's see this at work for our conversion function:
```haskell
ghci> :l converter01.hs
ghci> :i convert
convert :: Float -> Float -> Float -> Float
ghci> convert <$> (Just 1.34) <*> (Just 0.90) <*> (Just 10000)
Just 6716.4175
-- Let's step through it
ghci> p1 = convert <$> (Just 1.34)
ghci> :i p1
Maybe (Float -> (Float -> Float))
ghci> p2 = p1 <*> (Just 0.90)
ghci> :i p2
p2 :: Maybe (Float -> Float) 
ghci> p3 = p2 <*> (Just 10000)
ghci> :i p3
p3 :: Maybe Float
```

There are two things that may seem unsatisfactory:
1. In our actual application, we have `Maybe` for the rates, as the map lookup might fail. But we have an actual value for the amount we want to convert, not a `Maybe`.
2. More generally, the first application is treated differently (we apply `fmap/<$>` to get our function into the context). 

The first problem is pretty straightforward: If I have an `amount::Float` (not a `Maybe Float`) that I want to convert, I cannot use the `<*>` pattern directly. But what I can do is use `Just amount`, which make the amount I had and puts it into the `Maybe` context.

I can actually do this with the function `convert` as well! This function has type `Float -> Float -> Float -> Float`, but `Just convert` has type `Just (Float -> Float -> Float -> Float)`. I don't need to apply `fmap` to put the function into a context, I can apply `Just` to it! We can see this by looking at the example above
```haskell
ghci> :l converter01.hs
ghci> convert <$> (Just 1.34) <*> (Just 0.90) <*> (Just 10000)
Just 6716.4175
-- Note that we now have a chain of <*>
ghci> (Just convert) <*> (Just 1.34) <*> (Just 0.90) <*> (Just 10000)
Just 6716.4175
```
This motiviates the other function an applicative gives us: `pure`. It is a way of taking a "pure" value and putting it inside the applicative's context. In the case of `Maybe`, this is using `Just`. Rewriting again:
```haskell
ghci> (pure convert) <*> (Just 1.34) <*> (Just 0.90) <*> (Just 10000)
Just 6716.4175
-- Can also do to the 10_000 at the end
ghci> (pure convert) <*> (Just 1.34) <*> (Just 0.90) <*> (pure 10000)
Just 6716.4175
```
Using the "lifting" terminology, `pure` takes a value and "lifts" it into the context.

Let's give our final version of the currency calculator
```haskell
{{< include converter03.hs >}}
```

Running a couple of examples:
```haskell
ghci> :l converter03
ghci> convertCurrency (Currency "YEN" 10000) "RMB"
Just (Currency "RMB" 450.58325)
-- Giving an example of a currency we don't have the exchange rate of, Bitcoin
ghci> convertCurrency (Currency "YEN" 10000) "BTC"
Nothing
```

## Applicatives and laws

The main motivation for appliciatives are to use what we did with `fmap` (pushing functions of a single argument into a context) to other functions that take more arguments. The examples used the `Maybe` context extensively, but let's generalize this to other contexts.

For a formal defintion, an Applicative `context` is also a functor (i.e. already has `fmap`) with two additional functions:
* `<*> :: context (a->b) -> context a -> context b`
* `pure :: a -> context a`

In code, the definition is 
```haskell
class Functor context => Applicative context where 
    pure :: a -> context a
    (<*>) :: context (a -> b) -> context a -> context b
```

There are also five applicative laws (not enforced by the compiler, but enforced by convention). From wikipedia:
```haskell
1. pure id <*> v = v                            -- Identity
2. pure f <*> pure x = pure (f x)               -- Homomorphism
3. u <*> pure y = pure ($ y) <*> u              -- Interchange
4. pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
5. fmap f x = (pure f) <*> x
```
Let's start with the last one: the idea of the `<*>` operator is that it allows us to generalize `fmap`. We saw two different ways of doing this:

*  Use `fmap` once, to get the function into the context, and then use `<*>` on the remaining partial applications
* Use `(pure f)` to put the function into the context, and then use `<*>` for all the partial applications.

Rule 5 guarantees that these two approaches do the same thing.

The other 4 rules capture the idea that `pure` puts a value in a context, while making as few other changes as possible. In words

1. Make sure that lifting the identity function to a context gives you the identity function between contexts.
2. (Applying a function and then lifting) is the same as (lifting the function, lifting the value, and then using the lifted function to transform the lifted value)
4. Composition is associative.

To implement the Applicative for `Maybe` the code is
```haskell
instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure x                = Just x
    -- <*> :: Maybe (a->b) -> Maybe a -> Maybe b
    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing
```


## Style guide for applicatives: `liftA2`, `liftA3`, .., `liftAN`

We have already talked about how, given a binary function `binFunc` we can lift the function into an applicative context `Context` in two ways
```haskell
-- binFunc :: a -> b -> c
-- argInContext1 :: Context a
-- argInContext2 :: Context b
binFunc <$> argInContext1 <*> argInContext2
--example
(+) <$> (Just 5) <*> (Just 10)
-- Just 15

-- OR
(pure binFunc) <*> argInContext1 <*> argInContext2
-- example
(pure (+)) <*> (Just 5) <*> (Just 10)
-- Just 15
```

There is an alternative, which is to use
```haskell
import Control.Applicative (liftA2)

liftA2 :: (a->b->c) -> (Context a) -> (Context b) -> (Context c)
liftA2 binFunc x y = (pure binFunc) <*> x <*> y
```
That is, `liftA2` is exactly the function we wanted in the motivation section, except there we called it `fmap2`.

Similarly, there is a `liftA3` that does exactly what you would expect:
```haskell
liftA3: (a->b->c->d) -> Context a -> Context b -> Context c -> Context d
liftA3 triFunc x y z = (pure triFunc) <*> x <*> y <*> z
```

Stylewise, it is preferred to use `liftA2` and `liftA3` over `(pure ...) <*> .. <*> ..` or `(..) <$> .. <*> .. <*> ..` because the `liftAN` functions more clearly signals intent, and that the operators `<$>` and `<*>` can be difficult to parse. It starts to look like a regular expression.

## Example built-in applicatives

- Maybe
- Either
- IO


## Summary

* All applicatives are already functors.
* The functor `fmap` allows us to push a function of a single value into a context. The appliciative operator `<*>` allows us to push a function of multiple arguments into a context.
* For example, using `Maybe`, and the function `factorial :: n -> n`
```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


fmap factorial (Just 3)       --  Just 6
fmap factorial (Nothing)      --   Nothing

-- allow (+):: Int-> Int-> Int to be pure (+) :: Maybe Int -> Maybe Int -> Maybe Int
(pure (+)) <*> (Just 3) <*> (Just 6)   -- Just 9
(pure (+)) <*> (Just 3) <*> (Nothing)  -- Nothing
(+) <$> (Just 3) <*> (Nothing)  -- equivlent to above

-- Let's define a ternary function
f :: Int -> Int -> Int -> Int
f a b c = (a+b)*c

(pure f) <*> (Just 6) <*> (Just 7) <*> (Just 2) -- Just (6+7)*2 = Just 26
f <$> (Just 6) <*> (Just 7) <*> (Just 2) -- Just (6+7)*2 = Just 26
```
* For functions with 2 or 3 arguments, often `liftA2` and `liftA3` from `Control.Applicative` are used instead, to reduce the line noise from `<$>` and `<*>`
```haskell
-- these are all equivalent
(pure (+)) <*> (Just 3) <*> (Nothing)  -- Nothing
(+) <$> (Just 3) <*> (Nothing)         -- Nothing
liftA2 (+) (Just 3) (Nothing)          -- Nothing
-- liftA2 f a b = f <$> a <*> b 

-- these are all equivalent
(pure f) <*> (Just 6) <*> (Just 7) <*> (Just 2) -- Just (6+7)*2 = Just 26
f <$> (Just 6) <*> (Just 7) <*> (Just 2)        -- Just (6+7)*2 = Just 26
liftA3 f (Just 6) (Just 7) (Just 2)             -- Just (6+7)*2
-- liftA2 f a b c = f <$> a <*> b  <*> c
```
* Really useful when chaining lookups from Maps, where the results return `Maybe`!
* To use applicatives, you need to have all the values in the same context. If you have raw values, you can use `pure` to lift them into the context. We did this in the currency conversion example at the beginning of the chapter.

In this chapter we didn't look at the `List` type as an applicative, and instead devote the next chapter to that.

To extend a context `X` to an applicative, use
```haskell
instance Applicative X where
    -- pure :: a -> X a
    pure x                = ....
    -- <*> :: X (a->b) -> X a -> X b
    <*>                   = ....
```