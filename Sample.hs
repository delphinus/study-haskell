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
