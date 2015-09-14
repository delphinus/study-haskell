-- hi非負整数値の桁数
digits :: Int -> Int
digits = length . show

-- 数値の自乗
square :: Num a => a -> a
square = (^ 2)

caseOfFirstLetter :: String -> String
caseOfFirstLetter str =
  case str of
    "" -> ""
    (x:xs) -> if 'a' <= x && x <= 'z'
                then "lower"
                else if 'A' <= x && x <= 'Z'
                  then "upper"
                  else "other"

caseOfFirstLetter' :: String -> String
caseOfFirstLetter' "" = "empty"
caseOfFirstLetter' (x:xs)
  | inRange 'a' 'z' = "lower"
  | inRange 'A' 'Z' = "upper"
  | otherwise       = "other"
  where
    inRange lower upper = lower <= x && x <= upper

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

segments :: [a] -> [[a]]
segments = foldr (++) [] . scanr (\a b -> [a] : map (a:) b) []

sort [] = []
sort (x:xs) = sort [ a | a <- xs, a < x ] ++ x : sort [ a | a <- xs, x <= a]

goodSumWithNext :: Num c => [c] -> [c]
goodSumWithNext xs = zipWith (+) xs (tail xs)

badSumWithNext :: [Integer] -> [Integer]
badSumWithNext [] = []
badSumWithNext (a:b:xs) = a+b : badSumWithNext (b:xs)

appliedHead :: (t -> [a]) -> t -> Maybe a
appliedHead f x = case f x of
                    [] -> Nothing
                    a : _ -> Just a
