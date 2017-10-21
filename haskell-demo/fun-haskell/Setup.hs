doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else x*2) + 1

{-[1, 2, 3] ++ [4, 5]
"hello" ++ "world"
'a': " small cat"-}
