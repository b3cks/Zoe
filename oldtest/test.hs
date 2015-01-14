module Main (main) where

data T = Ts {f1, f2 :: Obj}

data Obj = Int Integer | S String

instance Show Obj where
	show (Int n) = show n
	show (S s) = show s

main :: IO()
main =
	let
		scan ('<':cs) tag = extract cs tag
		scan (_:cs) tag = scan cs tag
		scan [] tag = []
		extract ('>':cs) tag = (tag:(scan cs []))
		extract (c:cs) tag = extract cs (tag ++ [c])
		extract [] tag = []
		a = Ts {f1 = (Int 1), f2 = (Int 2)}
		b = a {f1 = (Int 2)}
	in
		-- print (tail (scan "fsadgaw<dm><a><b>" []))
		print (f1 b)
		
		