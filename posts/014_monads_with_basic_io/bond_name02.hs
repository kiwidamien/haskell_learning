bondLikeIntro :: String -> String
bondLikeIntro "" = ""
bondLikeIntro name = (last $ words name) ++ ". " ++ name
-- Examples:
-- bondLikeIntro "James Bond"    -- "Bond. James Bond"
-- bondLikeIntro "Homer Simpson" -- "Simpson. Homer Simpson"

-- Using >> to throw away the result, but keep going
coolUserIntro :: IO ()
coolUserIntro = putStrLn "Name?" >> (bondLikeIntro <$> getLine ) >>= putStrLn