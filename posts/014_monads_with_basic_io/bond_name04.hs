bondLikeIntro :: String -> String
bondLikeIntro "" = ""
bondLikeIntro name = (last $ words name) ++ ". " ++ name
-- Examples:
-- bondLikeIntro "James Bond"    -- "Bond. James Bond"
-- bondLikeIntro "Homer Simpson" -- "Simpson. Homer Simpson"

-- Using do notation, where <- attaches a name to the value "inside" the context
-- and returns the last statement.
coolUserIntro :: IO ()
--coolUserIntro = putStrLn "Name?" >> getLine >>= (\name -> putStrLn $ bondLikeIntro name)
coolUserIntro = do
    putStrLn "Name?" 
    name <- getLine
    putStrLn (bondLikeIntro name)