# Monad with IO

The previous chapter showed how `bind` or `>>=` / `=<<` allowed us do more with pushing functions into the `Maybe` context. It may have felt like overkill, because for a `Maybe` we always have the ability to operate within the `Maybe` context by using 

```haskell
g :: a -> b
g anA = ......

f :: Maybe a -> Maybe b
f maybeA = case maybeA of
    Just anA -> Just (g anA)
    Nothing -> Nothing
```

So while we don't "take the `a` out of `Maybe a`", we can use `case` to make a branch and operate on the `a` as above. It doesn't feel that much wierder than the bind solution `f maybeA = anA >>= g`.

When using the `IO`


## Example 
### Bond. James Bond.

Let's write a function that takes a string, breaks it into words, and then constructs a new string "Last word. Repeat sentence". If written as a name, it will take "Last Name. Full Name" as the output (here we literally mean last name, as in the last name written, which may or may not be the family name).

```haskell
bondLikeIntro :: String -> String
bondLikeIntro "" = ""
bondLikeIntro name = (last $ words name) ++ ". " ++ name

bondLikeIntro "James Bond"    -- "Bond. James Bond"
bondLikeIntro "Homer Simpson" -- "Simpson. Homer Simpson"
``` 

We can simulate getting IO from the user like this 
```haskell
ghci> :l bond_name.hs
ghci> name_from_user = pure "Marge Simpson"::IO String
ghci> :i name_from_user 
name_from_user :: IO String 
```
i.e. the `pure` has "lifted" the string into the `IO` context. 

How would we apply our function on an `IO String`? This is just fmap!
```haskell
-- Using IO as the context
-- <$> :: (a -> b) -> IO a -> IO b
-- We have a function String -> String, we want to apply, so
-- (<$>) bondLikeIntro :: IO String -> IO String
ghci> (<$>) bondLikeIntro name_from_user -- returns "Simpson. Marge Simpson"::IO String
ghci> bondLikeIntro <$> name_from_user   -- same return, but uses infix operator
```

Now let's say we wanted to print this out. The REPL will do this for us automatically, but this wouldn't work if we put it in a script. We want to use the function `putStrLn :: String -> IO ()`. 

* We have the `IO String`, `<$> bondLikeIntro name_from_user` that we want to print
* We have a function `String -> IO ()`
* So we have a monad like thing: `>>= :: (IO String) -> (String -> IO ()) -> (IO ())`

We can't "get" the string `"Simpson. Marge Simpson"` out of the `IO String`, but we can use the Monad to push `putStrLn` into the `IO` context the `IO String` already has:
```haskell
ghci> (>>=) (bondLikeIntro <$> name_from_user) putStrLn  -- returns IO ()
Simpson. Marge Simpson                                   -- This is a printed side effect
-- equivalent infix:
ghci> (bondLikeIntro <$> name_from_user) >>= putStrLn  -- returns IO ()
Simpson. Marge Simpson                                   -- This is a printed side effect
```

Finally, we can wrap this up into a total script as follows

```haskell
{{< include bond_name.hs >}}
```

We did have to include one extra piece in here (the `>>` part), which we will explain next.

## 