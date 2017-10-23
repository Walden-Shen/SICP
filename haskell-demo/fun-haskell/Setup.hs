doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else x*2) + 1

{-[1, 2, 3] ++ [4, 5]
"hello" ++ "world"
'a': " small cat"-}

boomBang xs = [ if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [x | x <- st, x `elem` ['A' .. 'Z']]
