import qualified Data.Map as M

zipToCity :: M.Map Int String
zipToCity = M.fromList [(95616, "Davis"), 
                        (94102, "San Francisco"), (94103, "San Francisco"), (94014, "San Francisco"),
                        (30033, "Atlanta"), (30301, "Atlanta"), (30307, "Atlanta")]
                        
cityToState :: M.Map String String
cityToState = M.fromList [("Davis", "CA"), ("San Francisco", "CA"), ("Atlanta", "GA")]


useZipCodeFindState :: Int -> Maybe String
useZipCodeFindState zipCode = case M.lookup zipCode zipToCity of
    Nothing -> Nothing
    Just cityName -> M.lookup cityName cityToState