bondLikeIntro :: String -> String
bondLikeIntro "" = ""
bondLikeIntro name = (last $ words name) ++ ". " ++ name

bondLikeIntro "James Bond"    -- "Bond. James Bond"
bondLikeIntro "Homer Simpson" -- "Simpson. Homer Simpson"


coolUserIntro :: IO ()
--coolUserIntro = putStrLn "Name?" >> getLine >>= (\name -> putStrLn $ bondLikeIntro name)
coolUserIntro = putStrLn "Name?" >> ( bondLikeIntro <$> getLine ) >>= putStrLn