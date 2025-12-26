import qualified Data.Map as M

data Currency = Currency String Float deriving (Show, Eq)

oneUSDTo :: M.Map String Float
oneUSDTo = M.fromList [
    ("USD", 1.00), ("YEN", 156.02), ("RMB", 7.03), 
    ("EUR", 0.850), ("WON", 1444.45), ("NZD", 1.71),
    ("AUD", 1.49), ("GBP", 0.74)]

convert :: Float -> Float -> Float -> Float
convert rateSrc rateDest valSrc = valInUSD * rateDest
  where valInUSD = valSrc / rateSrc