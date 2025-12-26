bondLikeIntro :: String -> String
bondLikeIntro "" = ""
bondLikeIntro name = (last $ words name) ++ ". " ++ name
-- Examples:
-- bondLikeIntro "James Bond"    -- "Bond. James Bond"
-- bondLikeIntro "Homer Simpson" -- "Simpson. Homer Simpson"

-- Make the flow more apparent with using a lambda function
coolUserIntro :: IO ()
coolUserIntro = putStrLn "Name?" >> getLine >>= (\name -> putStrLn $ bondLikeIntro name)
