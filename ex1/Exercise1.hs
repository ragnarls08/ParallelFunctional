

main :: IO ()
main = print $ msort [3,5,2,1] 

msort :: Ord a => [a] -> [a]
msort is = is