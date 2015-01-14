{-
ZRTE.hs for Zoe.v.0.3 as of November 2013
Paul Bailes, William Woodward, Khoa Tran School of ITEE UQ
-}

module ZRTE where
-------------------------------

-- Alonzo Run-Time Environment                                                        
-- kind of flattened G hierarchy
-- but with builtin folders
-------------------------------

-- file path (WW) and serial numbers identifying source point, incl. folder type and which generators
type Scd = (String, Int, Int) 
 
data Alobj =
	Apply Scd Alobj Alobj
	|
	Func Scd (Alobj -> Alobj)
	|
	Folder
		Scd -- serial numbers identifying the folder type
		Alobj -- pure function
		Alobj -- shower
	|
	Noshow
	|
	Fcomp Scd Alobj Alobj
	|
	Numprim Scd Integer -- WW Natural Number object
	|
	Strprim Scd String
	|
	Boolprim Scd Bool
	|
	Listprim Scd [Alobj]
	|
	Concat Scd [Alobj]
	|
	-- KT
	Neg Scd Alobj
	|
	And Scd [Alobj]
	|
	Or Scd [Alobj]
	|
	Not Scd Alobj
	|
	FoldPacket Scd Alobj Alobj Alobj
	|
	ServerDecl Scd String Alobj
	------------------------
	|
	Add Scd [Alobj] -- WW for adding Nums
	|
	Mul Scd [Alobj] -- WW for multiplying Nums
	|
	IndexOp Scd [Alobj]
	|
	Equal Scd Alobj
	|
	NotEqual Scd Alobj
	|
	LtEqual Scd Alobj
	|
	Lt Scd Alobj
	|
	GtEqual Scd Alobj
	|
	Gt Scd Alobj
	|
	CompareOp Scd [Alobj]
	|
	Tuple Scd [Alobj]
	|
	Ftype Scd  -- serial numbers identifying the folder type
	|
	Funcs Scd | Nums Scd | Strs Scd | Bools Scd | Lists Scd | Tuples Scd | Ftypes Scd | Ltypes Scd | Mtypes Scd | Ttypes Scd | Showop Scd Alobj | Anys Scd
	|
	Ltype Scd Alobj
	|
	Mtype Scd Alobj Alobj
	|
	Ttype Scd [Alobj]
	|
	Tcheck Scd Alobj Alobj
	|
	Namstup Scd [Alobj]
	|
	Nam Scd String
	|
	NamstupSelector Scd Alobj Alobj
	|
	NamstupUpdate Scd Alobj Alobj
	|
	Error Scd Alobj

-- Trying to fix tuple printing
--tupleShow obj = return (concatMap(\y -> if  y == ']' then ")" else [y]) (concatMap(\y -> if  y == '[' then "(" else [y]) (show (map (show . eval) obj))))

-- KT
instance Show Alobj where
	show (Numprim scd n) = show n
	show (Strprim scd s) = s
	show (Func scd@(s,_,_) f) = "function " ++ show scd
	show (Boolprim scd True) = "TRUE"
	show (Boolprim scd False) = "FALSE"
	show (Listprim scd xs) =  show xs
  	show (Tuple scd ts) = "(" ++ (showt ts) ++ ")"
		where
			showt [x1,x2] = (show x1) ++ "," ++ (show x2)
			showt (x:xs) = (show x) ++ ","  ++ (showt xs)
			showt [] = []
	show (Namstup scd nts) =
		let
			shownt (x1@(Tuple _ [(Nam _ n),value]):x2:xs) = (n ++ "=" ++ ((show .eval) value) ++ "," ++ (shownt (x2:xs)))
			shownt [Tuple _ [(Nam _ n1),value1]] = (n1 ++ "=" ++ ((show .eval) value1))
		in
			"{" ++ (shownt nts) ++ "}"
	show (Nums _) = "INT"
	show (Strs _) = "STRING"
	show (Bools _) = "BOOL"
	show (Lists _) = "LISTS"
	show (Tuples _) = "TUPLES"
	show (Ftypes scd@(c,_,_)) = show c
	show (Ltypes _) = "LTYPES"
	show (Mtypes _) = "MTYPES"
	show (Ttypes _) = "TTYOES"
	show (Anys _) = "ANY"
	show (Ltype _ t) = "[" ++ show t ++ "]"
	show (Ttype scd ts) = show (Tuple scd ts)
	show x = show (eval x)

run :: Alobj -> IO()
run ao = print (eval ao)


eval :: Alobj -> Alobj

-- the most important one
eval (Apply scd f x) = eva2 scd (eval f) (eval x)

eval (Folder scf f s) = Folder scf f (eval s) -- f will already have been eval-ed; s just maybe not

eval (Fcomp scc f1 f2) = Func scc (\x -> Apply scc f1 (Apply scc f2 x))

eval (Concat scc []) = error ("attempt to concatenate nothing around: " ++ show scc)

-- KT fix Concat
eval (Concat scc xs@(x:_)) =
	let
		unstr (Strprim scs cs) = cs
		unstr obj = error ("attempt to concatenate string with non-string around: " ++ show scc)
		unlst (Listprim scl xs) = xs
		unlst obj = error ("attempt to concatenate list with non-list around: " ++ show scc)
		evalconcatStr css = Strprim scc (concat (map (unstr . eval) css))
		evalconcatLst css = Listprim scc (concat (map (unlst . eval) css))
	in
		case (eval x) of
			(Listprim _ _) -> evalconcatLst xs
			(Strprim _ _) -> evalconcatStr xs
		
{-		
eval (Succ scc num1@(Numprim scd1 n1) num2@(Numprim scd2 n2)) = 
	Numprim scc (n1^n2)
-}

-- KT
eval (Neg scc num) =
	Numprim scc (negate ((unnum . eval) num))
	where
		unnum (Numprim scn ns) = ns
		unnum obj = error ("attempt to negate non-number " ++ show scc)
		
eval (Not scc bool) =
	Boolprim scc (not ((unbool . eval) bool))
	where
		unbool (Boolprim scn bs) = bs
		unbool obj = error ("non-boolean type " ++ show scc)
	
eval (And scc bss) =
	Boolprim scc (and (map (unbool . eval) bss))
	where
		unbool (Boolprim scn bs) = bs
		unbool obj = error ("non-boolean type " ++ show scc)
		
eval (Or scc bss) =
	Boolprim scc (or (map (unbool . eval) bss))
	where
		unbool (Boolprim scn bs) = bs
		unbool obj = error ("non-boolean type " ++ show scc)	
		
-- WW Natural number addition and multiplication
-- eval (Add scc nss@((Numprim _ _):_)) =
-- KT fixing: evaluate subtree first
eval (Add scc nss) =
	Numprim scc (sum (map (unnum . eval) nss))
	where
		unnum (Numprim scn ns) = ns
		unnum obj = error ("attempt to add non-number obj" ++ show scc)
		
eval (Mul scc nss) =
	Numprim scc (product (map (unnum . eval) nss))
	where
		unnum (Numprim scn ns) = ns
		unnum obj = error ("attempt to multiply non-number obj" ++ show scc)

-- KT 
eval (CompareOp scc xs) =
	let
		evalCompNum [x1,op] =
			case op of
				(Equal cl x2) ->
					let
						n1 = unnum (eval x1) cl
						n2 = unnum (eval x2) cl
					in
						n1 == n2
				(NotEqual cl x2) ->
					let
						n1 = unnum (eval x1) cl
						n2 = unnum (eval x2) cl
					in
						n1 /= n2
				(Lt cl x2) ->
					let
						n1 = unnum (eval x1) cl
						n2 = unnum (eval x2) cl
					in
						n1 < n2
				(Gt cl x2) ->
					let
						n1 = unnum (eval x1) cl
						n2 = unnum (eval x2) cl
					in
						n1 > n2
				(LtEqual cl x2) ->
					let
						n1 = unnum (eval x1) cl
						n2 = unnum (eval x2) cl
					in
						n1 <= n2
				(GtEqual cl x2) ->
					let
						n1 = unnum (eval x1) cl
						n2 = unnum (eval x2) cl
					in
						n1 >= n2
						
		evalCompStr [x1,op] =
			case op of
				(Equal cl x2) ->
					let
						n1 = unstr (eval x1) cl
						n2 = unstr (eval x2) cl
					in
						n1 == n2
				(NotEqual cl x2) ->
					let
						n1 = unstr (eval x1) cl
						n2 = unstr (eval x2) cl
					in
						n1 /= n2
				(Lt cl x2) ->
					let
						n1 = unstr (eval x1) cl
						n2 = unstr (eval x2) cl
					in
						n1 < n2
				(Gt cl x2) ->
					let
						n1 = unstr (eval x1) cl
						n2 = unstr (eval x2) cl
					in
						n1 > n2
				(LtEqual cl x2) ->
					let
						n1 = unstr (eval x1) cl
						n2 = unstr (eval x2) cl
					in
						n1 <= n2
				(GtEqual cl x2) ->
					let
						n1 = unstr (eval x1) cl
						n2 = unstr (eval x2) cl
					in
						n1 >= n2
					
		unnum (Numprim scn ns) cl = ns
		unnum obj cl = error ("Integer expected " ++ show cl)
		
		unstr (Strprim scn ns) cl = ns
		unstr obj cl = error ("String expected " ++ show cl)
		
		getOperand (Equal _ x) = x
		getOperand (NotEqual _ x) = x
		getOperand (Lt _ x) = x
		getOperand (LtEqual _ x) = x
		getOperand (GtEqual _ x) = x
		getOperand (Gt _ x) = x
		
		evalNums [x1,op] = evalCompNum [x1,op] 
		evalNums (x1:op:ops1:ops2) = ((evalCompNum [x1,op]) && (evalNums ((getOperand op):ops1:ops2)))
		
		evalStrs [x1,op] = evalCompStr [x1,op] 
		evalStrs (x1:op:ops1:ops2) = ((evalCompStr [x1,op]) && (evalStrs ((getOperand op):ops1:ops2)))
	in
	do
		case eval (head xs) of
			(Numprim _ _) -> Boolprim scc (evalNums xs)
			(Strprim _ _) -> Boolprim scc (evalStrs xs)
			
-- KT implementing indexing op
eval (IndexOp scc [l,i]) =
	((unlist (eval l) scc) !! (unnum (eval i) scc))
	where
		unlist (Listprim _ ls) cl = ls
		unlist obj cl = error ("List expected on left hand side of '!!'" ++ show cl)
		unnum (Numprim scn ns) cl = fromIntegral ns
		unnum obj cl = error ("Integer expected on right hand side of '!!' " ++ show cl)
		
eval (IndexOp scc (l:i1:i2:is)) =
	eval (IndexOp scc (((unlist (eval l) scc) !! (unnum (eval i1) scc)):i2:is))
	where
		unlist (Listprim _ ls) cl = ls
		unlist obj cl = error ("List expected on left hand side of '!!'" ++ show cl)
		unnum (Numprim scn ns) cl = fromIntegral ns
		unnum obj cl = error ("Integer expected on right hand side of '!!' " ++ show cl)

eval (Tcheck sct x t) = evt2 sct (eval x) (eval t)

eval (Error scd s) = eve2 scd (eval s)

-- KT implement Fold syntax in run time
eval (FoldPacket scd folder ftype@(Ftype (fts,_,_)) svs) =
	let
		scan ('<':cs) tag = extract cs tag
		scan (_:cs) tag = scan cs tag
		scan [] tag = []
		extract ('>':cs) tag = (tag:(scan cs []))
		extract (c:cs) tag = extract cs (tag ++ [c])
		extract [] tag = []
		contructors = (scan fts [])
		getserver nt sn = 
			let 
				r = eval (NamstupSelector scd nt (Nam scd sn))
			in
				case r of
					(Error _ _) -> eval (NamstupSelector scd nt (Nam scd "zdefault"))
					val -> val
		buildServerList (c:cs) = (getserver svs c):(buildServerList cs)
		buildServerList [] = []
		evalapplies [x1,x2] = eval (Apply scd x1 x2)
		evalapplies (x1:x2:args) = evalapplies ((eval (Apply scd x1 x2)):args)
	in
	do
		evalapplies ([folder]++(buildServerList contructors))

-- KT		
eval (NamstupSelector scd nt target@(Nam _ n)) =
	let
		find((Tuple _ [(Nam _ x),value]):ts) =
			if x == n
			then value
			else find ts
		find [] = Error scd (Strprim scd (" No field with name \"" ++ n ++ "\" exists "))
	in
		case eval nt of
			(Namstup _ tuples) -> find tuples
			_ -> error ("Named tuple expected on left side of \'>>\'" ++ show scd)

-- KT
eval (NamstupUpdate scd nt1 nt2) =
	let
		tt@(t1,t2) = (eval nt1, eval nt2)
		update (x@(Tuple clt [(Nam cln n),_]):xs) y@(Tuple _ [fn@(Nam _ n1),value]) =
			if n == n1
			then ((Tuple clt [(Nam cln n),value]):xs)
			else (x:(update xs y))
		update [] t@(Tuple _ [(Nam _ n1),value]) = [t] -- add missing field
		update1 x (y:ys) = update1 (update x y) ys
		update1 x [] = x
	in
		case tt of
			((Namstup _ ts1),(Namstup _ ts2)) -> (Namstup scd (update1 ts1 ts2))
			((Namstup _ ts1),_) -> error ("Named tuple expected on right side of \'<<\' " ++ show scd)
			(_,(Namstup _ ts2)) -> error ("Named tuple expected on left side of \'<<\' " ++ show scd)
			(_,_) -> error ("Named tuple expected on both side of \'<<\' " ++ show scd)

-- KT implement SHOW in ZOE
eval (Showop scd n@(Numprim _ _)) = Strprim scd (show n)
eval (Showop scd s@(Strprim _ _)) = Strprim scd (show s)
eval (Showop scd f@(Func _ _)) = Strprim scd (show f)
eval (Showop scd b@(Boolprim _ _)) = Strprim scd (show b)
eval (Showop scd l@(Listprim _ _)) = Strprim scd (show l)
eval (Showop scd t@(Tuple _ _)) = Strprim scd (show t)
eval (Showop scd nt@(Namstup _ _)) = Strprim scd (show nt)

-- the rest!
-- Func
-- Noshow
-- Numprim
-- Strprim
-- Boolprim
-- Listprim
-- Tuple
-- Ftype
-- Ltype
-- Mtype
-- TType
-- Funcs Nums Strs Bools Lists Tuples Ftypes Ttypes Mtypes Ttypes Showop
-- Charprim

eval x = x

-- subsidiary: eval (Apply scd f x) = eva2 scd (eval f) (eval x)
eva2 sca (Func scf f) x = eval (f x)

eva2 sca (Folder scf f s) x = eva2 sca f x

-- WW Comment: Composes f applied to x n times
eva2 sca (Numprim scn n) f = Func sca (\ x -> eviter scn n f x)

eva2 sca (Boolprim scd True) x = Func sca (\y -> x)
eva2 sca (Boolprim scd False) x = Func sca (\y -> y)

eva2 sca (Listprim scd xs) o = Func sca (\b -> evlfold scd xs o b)

{-
old semantics for strings
-- s (s ++ s') y n = y s s'
-- s s' y n = n s s'
"qaz" "qazwsx" --> (\ y n -> y "qaz" "wsx")
"qaz" "qwerty" --> (

\ y n -> n "qaz" "qwerty")
-}

{-
new semantics for strings

s (s ++ s') y n = y s' (\ x y -> if null s' then y else x)
s s' y n = n s' (\ x y -> x y if s < s' then x else if s > s' then y else impossible)

then would be e.g.

tok s = (\ s' -> s s' (\ residue comp -> [residue]) (\residue comp -> []) )

s1=s2 = s1 s2 (\ residue comp -> not comp) (\residue comp -> F) )
s1!=s2 = s1 s2 (\ residue comp -> comp) (\residue comp -> T) )
s1>=s2 = s1 s2 (\ residue comp -> F) (\residue comp -> not comp) )
s1<=s2 = s1 s2 (\ residue comp -> T) (\residue comp -> T) )
s1>s2 = s1 s2 (\ residue comp -> F) (\residue comp -> not comp) )
s1<s2 = s1 s2 (\ residue comp -> comp) (\residue comp -> comp) )
-}

eva2 sca str1@(Strprim scd1 s1) str2@(Strprim scd2 s2)=
	let
		prefix [] ys = True
		prefix (x:xs) [] = False
		prefix (x:xs) (y:ys) = if x==y then prefix xs ys else False
		chop [] ys = ys
		chop (x:xs) [] = []
		chop (x:xs) (y:ys) = if x==y then chop xs ys else y:ys
	in
		Func sca (\y ->
			Func sca (\n ->
				if
					prefix s1 s2
				then
					-- old
					-- Apply sca (Apply sca y str1) (Strprim sca (chop s1 s2))
					-- new: y (chop s1 s2) (not (s1 == s2))
					Apply sca (Apply sca y (Strprim sca (chop s1 s2))) (Boolprim sca (not (s1 == s2)))
				else
					-- old
					-- Apply sca (Apply sca n str1) str2
					-- new: n s2 (s1 < s2)
					Apply sca (Apply sca n str2) (Boolprim sca (s1 < s2))
			)
		)

eva2 sca str1@(Strprim scd s1) str2 =
	error ("can only apply a string to a string, around: " ++ show sca)

-- KT implementing behavior of tuple
eva2 sca tup@(Tuple cl ts) func@(Func _ f) = apply func ts
	where 
		apply f (x1:x2:xs) = apply (eva2 sca f x1) (x2:xs)
		apply f [x] = eva2 sca f x
		
-- KT implementing behavior of named tuple
eva2 sca nt@(Namstup cln xs) str@(Strprim _ s1) =
	let
		chop (' ':xs) cap n r = if cap then chop xs False [] (r++[n]) else chop xs cap n r
		chop (x:xs) cap n r = chop xs True (n++[x]) r
		chop [] cap n r = if cap then (r++[n]) else r
		argList = chop s1 False [] []
		buildTup (x:xs) = (eval (NamstupSelector cln nt (Nam cln x))):(buildTup xs)
		buildTup [] = []
	in
		Tuple cln (buildTup argList)
		
		
eva2 sca (Ftype scf1) (Folder scf2 f s) = Boolprim sca (scf1==scf2)
eva2 sca (Ftype scf) x = falseprim
eva2 sca (Funcs scfs) (Func scf f) =  trueprim
eva2 sca (Funcs scfs) (Nums scns) =  trueprim 
eva2 sca (Funcs scfs) (Bools scbs) =  trueprim
eva2 sca (Funcs scfs) (Lists scls) =  trueprim
eva2 sca (Funcs scfs) (Strs scls) =  trueprim
eva2 sca (Funcs scfs) (Tuples scls) =  trueprim
eva2 sca (Funcs scfs) (Ftypes scfts) =  trueprim
eva2 sca (Funcs scfs) (Mtypes scms) =  trueprim
eva2 sca (Funcs scfs) (Ttypes scts) =  trueprim
eva2 sca (Funcs scfs) (Ltypes scls) =  trueprim
eva2 sca (Funcs scfs) x =  falseprim
eva2 sca (Nums scns) (Numprim scn n) = trueprim 
eva2 sca (Nums scns) x =  falseprim
eva2 sca (Strs scns) (Strprim scs s) = trueprim
eva2 sca (Strs scns) x =  falseprim
eva2 sca (Bools scbs) (Boolprim scb b) = trueprim
eva2 sca (Bools scbs) x =  falseprim
eva2 sca (Lists scls) (Listprim scl xs) = trueprim
eva2 sca (Lists scls) x =  falseprim
eva2 sca (Tuples scls) (Tuple sct xs) = trueprim
eva2 sca (Tuples scls) x =  falseprim
eva2 sca (Ftypes scfts) (Ftype scf) = trueprim
eva2 sca (Ftypes scfts) x =  falseprim
eva2 sca (Ltypes scls) (Ltype scl t) = trueprim
eva2 sca (Ltypes scls) x =  falseprim
eva2 sca (Mtypes scms) (Mtype scm d c) = trueprim
eva2 sca (Mtypes scms) x =  falseprim
eva2 sca (Ttypes scts) (Ttype sct ts) = trueprim
eva2 sca (Ttypes scts) x =  falseprim

-- eva2 sca (Showop scs) (Folder scf f s) = eva2 sca f s
-- eva2 sca (Showop scs) x = error ("can only SHOW a Folder, around: " ++ show sca)

eva2 sca (Ltype scl t) x = error ("Type error, Listprim expected, around: " ++ show sca)

eva2 sca (Mtype scm d c) x = error ("can't apply a mapping type, around: " ++ show sca)

eva2 sca (Ttype sct ts) x = error ("Type error, Tuple expected, around: " ++ show sca)

-- eva2 sca (Charprim scc c) x = error ("can't apply a string element, around: " ++ show sca) 

eva2 sca f x = error ("unknown rator around: " ++ show sca)

-- WW Comment: This function builds, recursively, the function composition for natural numbers
-- sub. for eva2 (Apply sca (Numprim scn n) f) = Func sca (\ x -> eviter scn n f x)
eviter scd 0 f x = x
eviter scd n f x = eval (Apply scd f (eviter scd (n-1) f x))

-- sub. for eva2 (Apply sca (Listprim scd xs) o) = Func (\b -> evlfold scd xs o b)
evlfold scd [] o b = b
evlfold scd (x:xs) o b = eval (Apply scd (Apply scd o x) (evlfold scd xs o b))

--STARTTW--
evac scc css@((Strprim _ _):_) =
  Strprim scc (concat (map unstr css))
  where
    unstr (Strprim scs cs) = cs
    unstr obj = error ("attempt to concatenate string with non-string around: " ++ show scc)
evac scc xss@((Listprim _ _):_) =
  Listprim scc (concat (map unlst xss))
  where
    unlst (Listprim scl xs) = xs
    unlst obj = error ("attempt to concatenate list with non-list around: " ++ show scc)
--ENDTW--

-- KT
evt2 sct x (Anys _) = x
	
-- KT implement type checking for user defined folder type
evt2 sct folder@(Func scf@(tag1,_,_) f) (Ftype scft@(tag2,_,_)) =
	if (check (scan tag1 []) (scan tag2 []))
	then folder
	else error ("folder type mismatch around " ++ show sct ++ "\nexpected type " ++ tag2 ++ "\nactual type " ++ tag1)
	where
		check [x] (y:ys) =
			if x == y
			then True
			else check [x] ys
		check x [] = False
		scan ('<':cs) tag = extract cs tag
		scan (_:cs) tag = scan cs tag
		scan [] tag = []
		extract ('>':cs) tag = (tag:(scan cs []))
		extract (c:cs) tag = extract cs (tag ++ [c])
		extract [] tag = []

------
evt2 sct (Listprim scl xs) ltype@(Ltype sclt t) =
	Listprim scl (map (\x -> Tcheck sct x t) xs)
	
evt2 sct f (Mtype scm d c) = (Func sct (\x -> eval (Tcheck sct (Apply sct f (Tcheck sct x d)) c)))

evt2 sct (Tuple scv ts) (Ttype sctt tts) =
    Tuple scv (map matchtt (zip ts tts))
    where
    matchtt (tx,tt) =  Tcheck scv tx tt
	
-- KT type ckeck Listprim and (named) Tuple
evt2 sca (Listprim scd1 xs) (Listprim scd2 [t]) =
	Listprim scd1 (map (\x -> Tcheck scd1 x t) xs)
	
evt2 sca t@(Namstup cln _) (Namstup _ nt1) =
	let
		check (x@(Tuple cl1 [(Nam cl2 n), ttype]):xs) =
			(Tuple cl1 [(Nam cl2 n), (Tcheck cl1 (eval (NamstupSelector sca t (Nam sca n))) ttype)]):(check xs)
		check [] = []
	in
		Namstup cln (check nt1)
	
evt2 sca (Tuple scd1 t1) (Tuple scd2 t2) =
	Tuple scd1 (map matchtt (zipt t1 t2))
	where
	zipt (x1:xs1) (x2:xs2) = (x1,x2):(zipt xs1 xs2)
	zipt (x1:xs1) [] = error ("type error around " ++ show sca ++ "\nExpected type: " ++ show (Tuple scd2 t2))
	zipt [] (x2:xs2) = error ("type error around " ++ show sca ++ "\nExpected type: " ++ show (Tuple scd2 t2))
	zipt [] [] = []
	matchtt (t1,t2) = Tcheck scd1 t1 t2
--

evt2 sct x f =
	case eva2 sct f x of
		(Boolprim scb True) -> x
		(Boolprim scb False) -> error ("type error around " ++ show sct ++ "\nExpected type: " ++ show f)
		xx -> error ("malformed type for check around " ++ show sct)
-- won't ever get here while the above catches all
-- evt2 sct x t = error ("value-type mismatch for check around " ++ show sct)

-- subsidiary: eval (Error scd s) = eve2 scd (eval s)
eve2 scd (Strprim scs s) = error ("zoe error:" ++ s ++ " around " ++ show scd)
eve2 scd x = error ("zoe error (with failed error message) around " ++ show scd)

libcd = ("ZRTE",0,0)
trueprim = Boolprim libcd True
falseprim = Boolprim libcd False