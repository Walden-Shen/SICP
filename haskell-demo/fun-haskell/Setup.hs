doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else x*2) + 1

{-[1, 2, 3] ++ [4, 5]
"hello" ++ "world"
'a': " small cat"-}

boomBang xs = [ if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [x | x <- st, x `elem` ['A' .. 'Z']]

rightTriangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a^2 + b^2 == c^2]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

{- read "5" + 5 
   read "5" :: Int -}

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe x = "Bad!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 
