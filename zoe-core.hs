{-
zoe-core.hs for Zoe.v.0.3 as of November 2013
Paul Bailes, William Woodward, Khoa Tran School of ITEE UQ
-}

import System.Environment (getArgs)
import Data.Char (toUpper)
import System.Directory
-- derived from sysu Jun-Jul 2011 parsing library #2
------------------------
-- PARSING INFRASTRCTURE
------------------------

type Coords = (String,Int,Int)
type Prsr r = (Coords, String) -> [((Coords, String), r)]

parse :: String -> (Prsr r) -> String -> [((Coords, String), r)]
-- Linux version
-- parse currentFilePath p s = p (((concatMap(\y -> if (y == '/' || y == '.' || y == ' ') then "_" else [y]) currentFilePath),1,1),s)
-- Windows version
parse currentFilePath p s = p (((concatMap(\y -> if (y == '\\' || y == ':' || y == ' ' || y == '/' || y == '.') then "_" else [y]) currentFilePath),1,1),s)

emty :: r -> Prsr r
emty n s = [(s, n)]

tk :: String -> Prsr Ptree
tk t ((cfp,c,l),s) = if prefix t s then [(chop cfp c l t s, Emty (cfp,c,l))] else []
tkp :: String -> (Coords -> Ptree) -> Prsr Ptree
tkp t p ((cfp,c,l),s) = if prefix t s then [(chop cfp c l t s, p (cfp,c,l))] else []

prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = if x==y then prefix xs ys else False

-- copy of Zoe-specifics for comparison
-- spaces :: Prsr String
-- spaces ((c,l),' ':cs) = spaces ((c+1,l),cs)
chop cfp c l [] (' ':ys) = chop cfp (c+1) l [] ys
-- spaces ((c,l),'\n':cs) = spaces ((1,l+1),cs)
chop cfp c l [] ('\n':ys) = chop cfp 1 (l+1) [] ys
-- spaces ((c,l),'\t':cs) = spaces ((1,l+1),cs)
chop cfp c l [] ('\t':ys) = chop cfp (c+4) l [] ys -- KT changes to +4 for tab

-- Line comment
chop cfp c l [] ('-':'-':ys) = untilnlt cfp (c+2) l ys
-- KT block comment (not yet nested block)
chop cfp c l [] ('{':'-':ys) = untilebt cfp (c+2) l ys

chop cfp c l [] ys = ((cfp,c,l),ys)
chop cfp c l (x:xs) [] = ((cfp,c,l),[])
chop cfp c l (x:xs) (y:ys) = if x==y then chop cfp (c+1) l xs ys else ((cfp,c,l),y:ys)

untilnlt cfp c l [] = ((cfp,c,l),[])
untilnlt cfp c l ('\n':cs) = chop cfp 1 (l+1) [] cs
untilnlt cfp c l (_:cs) = untilnlt cfp (c+1) l cs

-- KT block comment
untilebt cfp c l [] = ((cfp,c,l),[])
untilebt cfp c l ('-':'}':cs) = chop cfp (c+2) l [] cs
untilebt cfp c l ('\n':cs) = untilebt cfp 1 (l+1) cs
untilebt cfp c l (_:cs) = untilebt cfp (c+1) l cs

-- catch-all
-- spaces ((c,l),cs) = [(((c,l),cs), " ")]

alt :: Prsr r -> Prsr r -> Prsr r
-- p1 `alt` p2 = \s -> p1 s ++ p2 s
-- hack to stop multiple parses
-- (1) avoid lexical coding for <name>
-- (2) efficiency?
p1 `alt` p2 =
	(\cls ->
		let
			r1 = p1 cls
		in
			if null r1
			then p2 cls
			else r1
	)

-- Takes a list of parsers and returns a single parser (Ptree) (I think?)
cat :: [Prsr q] -> ([q] -> Coords -> r) -> Prsr r
cat [] pn s = error "malformed parser"
cat ps pn cls@(cl,_) =
	-- putting the result together
	(\clsnts_s ->
		let
			(clss, tss) = unzip clsnts_s
		in
			zip clss (map (\ ts -> pn ts cl) tss)
	)
	-- applied to
	(	
	-- iterating over the cated parsers
		foldr

		-- combining in the next cat-ed parser
		(\p clsnts_s ->
			let
				(clss,tss) = unzip clsnts_s
				clsnt_s_s = map p clss
			in
				concat(
					map
					(\(clsnt_s,ts) ->
						map
						(\(cls,t) -> (cls, ts++[t]))
						clsnt_s				
					)
					(zip clsnt_s_s tss)
				)
		)

		-- setting up the base of the fold
		(	let
				(clss,ts) = unzip (head ps cls)
			in
				zip clss (map (:[]) ts)
		)

		-- list of parsers being folded over
		((reverse . tail) ps) 

	) -- end of the fold

-- some new operators
list1 :: Prsr r -> Prsr [r]
list1 p =
	cat [onelt p, list0 p] (\ (x1:x2:_) cl -> x1 ++ x2)
	`alt`
	onelt p

list0 :: Prsr r -> Prsr [r]
list0 p =
	cat [onelt p, list0 p] (\ (x1:x2:_) cl -> x1 ++ x2)
	`alt`
	emty []

-- Has the effect of turning a parse tree node into a 1-element parse tree
onelt :: Prsr r -> Prsr [r]
-- onelt p = cat [p] (: []) -- wrong (but why???)
onelt p = liftp p (\ cl x1 -> [x1])

-- Takes a parser - parses it as a 1-element list
liftp :: Prsr q -> (Coords -> q -> r) -> Prsr r
liftp p lifter = cat [p] (\ [x1] cl -> lifter cl x1)

----------------------------
-- Alonzo abstract syntax --
----------------------------

data Ptree =
	MainProg Coords Ptree [Ptree] [Ptree]		-- <some kind of expr> [@Decl] [@ModImp]
	| ModProg Coords Ptree [Ptree] [Ptree]		-- @Nam [@Decl] [@ModImp]
	| Prog Coords Ptree [Ptree]					-- <some kind of expr> [@Decl]
	| Decl Coords [Ptree] Ptree					-- [@Nam] @Prog
	| ModImp Coords Ptree						-- @Nam
	| FTdecl Coords Ptree [Ptree] 				-- @Nam [@NFalt] No need for show right now WW 14/10
	| NFalt Coords Ptree [Ptree] 				-- @Nam [@Fany | @Frec] Moved back by WW 14/10
	| Falt Coords [Ptree]						-- [@Fany | @Frec] WW 14/10
	| Fany Coords Ptree							-- KT adds Ptree
	| Frec Coords								-- Moved back by WW 14/10
	| Lcat Coords [Ptree]						-- [<some kid of expr>]
	| Fcomp Coords [Ptree]						-- [<some kid of expr>]
	| Tcheck Coords [Ptree]						-- [<some kid of expr>]
	| Tmap Coords [Ptree]						-- [<some kid of expr>]
	| Tprod Coords [Ptree]						-- [<some kid of expr>]
	| Fappl Coords [Ptree]						-- [<some kid of expr>]
	------------ KT adding new ops ------------------
	| Nneg Coords Ptree					
	| Not Coords Ptree					
	| BoolOr Coords [Ptree]				
	| BoolAnd Coords [Ptree]
	| IndexOp Coords [Ptree]
	------------ KT adding new Fold syntax ----------
	| FoldPacket Coords Ptree Ptree Ptree
	------------ KT adding CompareOp ----------------
	| CompareOp Coords [Ptree]
	-------------------------------------------------
	| Equal Coords Ptree
	| NotEqual Coords Ptree
	| LessEqual Coords Ptree
	| Less Coords Ptree
	| GreaterEqual Coords Ptree
	| Greater Coords Ptree
	------------------------------------------------
	| Nadd Coords [Ptree]				
	| Nmul Coords [Ptree]
	--------------- START TYPE ---------------------
	| Recn Coords 
	| Univ Coords
	| Btm Coords
	| Bval Coords Bool
	| Shows Coords Ptree
    | Funcs Coords
    | Nums Coords
    | Strs Coords
    | Bools Coords
    | Lists Coords
    | Tuples Coords
    | Ftypes Coords
	| Anys Coords								-- KT adds (type of any)
    | Ltypes Coords Ptree						-- KT modifies
    | Mtypes Coords
    | Ttypes Coords [Ptree]						-- KT modifies
	--------------- END TYPE ------------------------
	| Listprim Coords [Ptree]
	| Nam Coords String
	| Error Coords Ptree
	| Emty Coords
	| Func Coords [Ptree] Ptree
	| Str Coords String
	| Nat Coords String 						-- WW Added to tree for natural numbers 11/05
	| Tuple Coords [Ptree]
	| IfThenElse Coords Ptree Ptree Ptree 		-- WW Added to tree for IF THEN ELSE ENDIF 02/09
	--------- KT adding pattern matching -------------
	| Pmatch Coords Ptree Ptree
	| PmTuple Coords [Ptree]
	| PmZlist Coords [Ptree]
	| PmZlistht Coords [Ptree] 					-- for pattern matching e.g. (head:tail)
	--------- KT adding named tuple ------------------
	| Namstup Coords [Ptree]
	| NamstupSelector Coords [Ptree]
	| NamstupUpdate Coords [Ptree]
	deriving Show
	
-------------------------
-- Alonzo concrete syntax
-------------------------

toplvl :: Prsr Ptree
toplvl =
	cat [onelt (tk "MODULE"), onelt (liftp name Nam), decls, modimps] (\ [_,[x2],x3,x4] cl -> ModProg cl x2 x3 x4)
	`alt`
	cat [onelt expr, decls, modimps] (\ [[x1],x2,x3] cl -> MainProg cl x1 x2 x3)
	
-- prog = expr [WHERE {decl}]
prog :: Prsr Ptree
prog =
	cat [onelt expr, decls] (\ [[x1],x2] cl -> Prog cl x1 x2)

modimps :: Prsr [Ptree]
modimps = list0 (cat [tk "IMPORT", liftp name Nam, tk ";"] (\ [_,x2,_] cl -> ModImp cl x2))

decls :: Prsr [Ptree]
decls =
	cat [onelt (tk "WHERE"), list1 decl] (\ [_,x2] cl -> x2) -- WHERE (caps) so as to avoid clash with <name>
	`alt`
	emty []

-- decl = namstr+ '=' prog ';'  | 'TYPE' name  = foldelt* ('||' foldelt*)* ';'
-- TYPE definition code adapted by WW from original non-functional implementation
decl :: Prsr Ptree
decl =
	cat [list1 namstr, list1 (tk "="), onelt prog, onelt (tk ";")] (\ [x1,_,[x3],_] cl -> Decl cl x1 x3)
	`alt`
	-- KT fixs: get cln from x5 pass to the first NFalt
	cat [
		onelt (tk "TYPE"), onelt (liftp name Nam),  list1 (tk "="),
		onelt (liftp name Nam), list0 nfoldelt, list0 nfoldopt, onelt (tk ";")
		]
		(\ (_:[x2]:_:[x5@(Nam cln _)]:x6:x7:_) cl ->
			FTdecl cl x2 ((NFalt cln x5 x6):x7)
		)
		
-- WW Adapted from old implementation
nfoldopt :: Prsr Ptree -- @NFalt
nfoldopt = cat [onelt (tk "|"), onelt (liftp name Nam), list0 nfoldelt] (\ [_,[x2],x3] cl -> NFalt cl x2 x3)

-- KT modifies for coding consistency
nfoldelt :: Prsr Ptree -- @Fany | @Frec
nfoldelt =
	cat [typesy] (\ [x] cl -> (Fany cl x))
	`alt`
	cat [tk "@"] (\ [x] cl -> (Frec cl))

-- 'IF' Ptree 'THEN' Ptree 'ELSE' Ptree 'FI'
-- IF THEN ELSE FI parsing infrastructure
-- Added by WW 2/09
ifThenElse :: Prsr Ptree
ifThenElse = 
	cat [
		tk "IF",
		prog, 
		tk "THEN", -- Keywords cannot be prefixes of each other!n 
		prog, 
		tk "ELSE", 
		prog,
		tk "FI"
		]
		(\ [_,cond,_,thenbranch,_,elsebranch,_] cl -> IfThenElse cl cond thenbranch elsebranch)

namstr :: Prsr Ptree -- @Nam | @Namstup
namstr =
	-- KT modify for pattern mathching
	cat [onelt (liftp name Nam), (onelt . tk) "@", onelt namstr] (\ [[x1],_,[x2]] cl -> Pmatch cl x1 x2)
	`alt`
	liftp name Nam
	`alt`
	cat [(onelt . tk) "(", onelt namstr, list1 pmtupelt, (onelt . tk) ")"] (\ [_,x2,x3,_] cl -> PmTuple cl (x2 ++ x3))
	`alt`
	cat [(onelt . tk) "(", onelt namstr, list1 pmzlistht, (onelt . tk) ")"] (\ [_,x2,x3,_] cl -> PmZlistht cl (x2++x3))
	`alt`
	cat [(onelt . tk) "[", (onelt . tk) "]"] (\ xs cl -> PmZlist cl [])
	`alt`
	cat [(onelt . tk) "[", onelt (liftp name Nam), list0 pmzlist, (onelt . tk) "]"] (\ [_,x2,x3,_] cl -> PmZlist cl (x2++x3))

-- KT
pmtupelt = cat [tk ",", namstr] (\ [_,x2] cl -> x2)
pmzlistht = cat [tk ":", namstr] (\ [_,x2] cl -> x2)
pmzlist = cat [tk ",", (liftp name Nam)] (\ [_,x2] cl -> x2)

cbinop :: ([Ptree] -> Ptree) -> Ptree -> [Ptree] -> Ptree
cbinop op x [] = x
cbinop op x xs = op (x:xs)

-- WW The following binary expressions are in order of precedence (lowest to highest)

expr = expr0

-- KT Logical op Or
expr0 :: Prsr Ptree
expr0 = cat [onelt expr1, list0 expr0b] (\ [[x1], x2] cl -> cbinop (BoolOr cl) x1 x2)
expr0b = cat [tk "||", expr1] (\ [_,x2] cl -> x2)

-- KT Logical op And
expr1 :: Prsr Ptree
expr1 = cat [onelt expr2, list0 expr1b] (\ [[x1], x2] cl -> cbinop (BoolAnd cl) x1 x2)
expr1b = cat [tk "&&", expr2] (\ [_,x2] cl -> x2)

-- KT re-design comparision op
expr2 :: Prsr Ptree
expr2 = cat [onelt expr8, list0 expr2b] (\ [[x1], x2] cl -> cbinop (CompareOp cl) x1 x2)
expr2b = 
	cat [tk "==", expr8] (\ [_,x2] cl -> (Equal cl x2))
	`alt`
	cat [tk "<=", expr8] (\ [_,x2] cl -> (LessEqual cl x2))
	`alt`
	cat [tk ">=", expr8] (\ [_,x2] cl -> (GreaterEqual cl x2))
	`alt`
	cat [tk "<", expr8] (\ [_,x2] cl -> (Less cl x2))
	`alt`
	cat [tk ">", expr8] (\ [_,x2] cl -> (Greater cl x2))
	`alt`
	cat [tk "!=", expr8] (\ [_,x2] cl -> (NotEqual cl x2))

-- KT fixs precedence
expr8 :: Prsr Ptree
expr8 = cat [onelt expr9, list0 expr8b] (\ [[x1], x2] cl -> cbinop (Nadd cl) x1 x2)
expr8b =
	cat [tk "+", expr9] (\ [_,x2] cl -> x2)
	`alt`
	cat [tk "-", expr9] (\ [_,x2] cl -> Nneg cl x2)

expr9 :: Prsr Ptree
expr9 = cat [onelt expr10, list0 expr9b] (\ [[x1], x2] cl -> cbinop (Nmul cl) x1 x2)
expr9b = cat [tk "*", expr10] (\ [_,x2] cl -> x2)

expr10 :: Prsr Ptree
expr10 = cat [onelt expr11, list0 expr10b] (\ [[x1], x2] cl -> cbinop (IndexOp cl) x1 x2)
expr10b = cat [tk "!!", expr11] (\ [_,x2] cl -> x2)

expr11 :: Prsr Ptree
expr11 = cat [onelt expr12, list0 expr11b] (\ [[x1], x2] cl -> cbinop (Lcat cl) x1 x2)
expr11b = cat [tk "++", expr12] (\ [_,x2] cl -> x2)

expr12 :: Prsr Ptree
expr12 = cat [onelt expr13, list0 expr12b] (\ [[x1], x2] cl -> cbinop (Fcomp cl) x1 x2)
expr12b = cat [tk ".", expr13] (\ [_,x2] cl -> x2)

expr13 :: Prsr Ptree
expr13 = cat [onelt expr14, list0 expr13b] (\ [[x1], x2] cl -> cbinop (Tcheck cl) x1 x2)
expr13b = cat [tk "::", typesy] (\ [_,x2] cl -> x2)

expr14 :: Prsr Ptree
expr14 = cat [onelt expr15, list0 expr14b] (\ [[x1], x2] cl -> cbinop (NamstupSelector cl) x1 x2)
expr14b = cat [tk ">>", liftp name Nam] (\ [_,x2] cl -> x2)

expr15 :: Prsr Ptree
expr15 = cat [onelt exprz, list0 expr15b] (\ [[x1], x2] cl -> cbinop (NamstupUpdate cl) x1 x2)
expr15b = cat [tk "<<", exprz] (\ [_,x2] cl -> x2)

exprz :: Prsr Ptree
exprz = liftp (list1 factor) (\ cl (x:xs) -> cbinop (Fappl cl) x xs)

factor :: Prsr Ptree
factor =
	liftp name Nam
	`alt`
	ifThenElse
	`alt`
	cat [tk "(", prog, tk ")"] (\ (_:x2:_) cl -> x2)
	`alt`
	cat [(onelt . tk) "(", onelt prog, list1 ptupelt, (onelt . tk) ")"] 
		(\ [_,x2,x3,_] cl -> Tuple cl (x2++x3))
	`alt`
	cat [onelt (tk "(\\"), list1 namstr, onelt (tk "->"), onelt prog, onelt (tk ")")] 
		(\ (_:x2:_:[x4]:_) cl -> Func cl x2 x4)
	`alt`
	cat [(onelt . tk) "[", (onelt . tk) "]"] (\ xs cl -> Listprim cl [])
	`alt`
	cat [(onelt . tk) "[", onelt prog, list0 ptupelt, (onelt . tk) "]"] 
		(\ [_,x2,x3,_] cl -> Listprim cl (x2++x3))
	`alt`
	namstup -- KT
	`alt`
	string
	`alt`
	num
	`alt`
	tkp "@" Recn
	`alt`
	tkp "U" Univ
	`alt`
	tkp "?" Btm
	`alt`
	cat [tk "ERROR", string] (\ [_,x2] cl -> Error cl x2)
    `alt`
    tkp "TRUE" (\ cl -> Bval cl True) `alt` tkp "FALSE" (\cl -> Bval cl False) -- KT
	`alt`
	cat [tk "(", tk "-", factor, tk ")"] (\ [_,_,x3,_] cl -> Nneg cl x3) -- KT
	`alt`
	cat [tk "(", tk "!", factor, tk ")"] (\ [_,_,x3,_] cl -> Not cl x3) -- KT
	`alt`
	cat [tk "(", tk "SHOW", factor, tk ")"] (\[_,x1,x2,_] cl -> Shows cl x2) -- KT
	`alt`
	cat [onelt (tk "FOLD"), onelt (liftp name Nam), onelt (tk "::"), onelt (liftp name Nam), onelt foldservers] 
		(\ [_,[x1],_,[x2],[x3]] cl -> FoldPacket cl x1 x2 x3) -- KT
		
foldservers = liftp name Nam `alt` namstup
	
-- KT: changing grammar and type lexcial tokens
typesy =
	liftp name Nam
	`alt`
	tkp "FUNC" Funcs
    `alt`
    tkp "INT" Nums
    `alt`
    tkp "STRING" Strs
    `alt`
    tkp "BOOL" Bools
    `alt`
    tkp "LISTS" Lists
    `alt`
	tkp "TUPLES" Tuples
    `alt`
    tkp "FTYPES" Ftypes
    `alt`
    tkp "MTYPES" Mtypes
    `alt`
	cat [(onelt . tk) "LTYPES", onelt typesy] (\ [_,[x1]] cl -> Ltypes cl x1)
    `alt`
	cat [(onelt . tk) "TTYPES", (onelt . tk) "(", onelt typesy, list1 typelt, (onelt. tk) ")"] (\ [_,_,x1,x2,_] cl -> Ttypes cl (x1++x2))
    `alt`
	tkp "ANY" Anys -- KT
	`alt`
	cat [(onelt . tk) "(", onelt typesy, list1 ptupetypelt, (onelt . tk) ")"] (\ [_,x2,x3,_] cl -> Tuple cl (x2++x3)) -- Tuple type
	`alt`
	cat [(onelt . tk) "[", onelt typesy, (onelt . tk) "]"] (\ [_,x2,_] cl -> Listprim cl x2) -- List Type
	`alt`
	cat [(onelt . tk) "{", onelt (liftp name Nam), (onelt . tk) "=", onelt typesy, list0 ntuptypelt, (onelt . tk) "}"]
	(\ [_,x2@[(Nam cln n)],_,[x4],x5,_] cl -> Namstup cl ((Tuple cln [(Nam cln n),x4]):x5))
	`alt`
	cat [(onelt . tk) "(",onelt typesy, list1 maptypelt,(onelt . tk) ")"] 
	(\ [_,x1,x2,_] cl -> Tmap cl (x1++x2))

typelt :: Prsr Ptree
typelt = cat [tk ",", typesy] (\ [_,x2] cl -> x2)

ptupelt :: Prsr Ptree
ptupelt = cat [tk ",", prog] (\ [_,x2] cl -> x2)

maptypelt = cat [tk "->", typesy] (\ [_,x2] cl -> x2)

namstup =
	cat [(onelt . tk) "{", onelt (liftp name Nam), (onelt . tk) "=", onelt prog, list0 ntuplt, (onelt . tk) "}"]
	(\ [_,x2@[(Nam cln n)],_,[x4],x5,_] cl -> Namstup cl ((Tuple cln [(Nam cln n),x4]):x5))

ntuplt = cat [(onelt . tk) ";", onelt (liftp name Nam), (onelt . tk) "=", onelt prog] (\ [_,[x2],_,[x4]] cl -> Tuple cl [x2,x4])

ptupetypelt :: Prsr Ptree
ptupetypelt = cat [tk ",", typesy] (\ [_,x2] cl -> x2)

ntuptypelt = cat [(onelt . tk) ";", onelt (liftp name Nam), (onelt . tk) "=", onelt typesy] (\ [_,[x2],_,[x4]] cl -> Tuple cl [x2,x4])

--------------------------------
-- misc lexical hacks for Alonzo
--------------------------------

alpha :: Prsr Char
alpha ((cfp,c,l),[]) = []
alpha ((cfp,c,l),ch:cs) =
	if ch >= 'a' && ch <= 'z'
	then [(((cfp,c+1,l),cs), ch)]
	else []

alphanum :: Prsr Char
alphanum ((cfp,c,l),[]) = []
alphanum ((cfp,c,l),ch:cs) =
	if ch >= 'a' && ch <= 'z' || ch >= '0' && ch <= '9' || ch == '_'
	then [(((cfp,c+1,l),cs), ch)]
	else []

-- WW Need to define the num lexical token 11/05
numchar :: Prsr Char
numchar ((cfp,c,l),[]) = []
numchar ((cfp,c,l),ch:cs) =
	if  ch >= '0' && ch <= '9'
	then [(((cfp,c+1,l),cs), ch)]
	else []

--Trying to define the number (num) syntax tree i.e. a call to Nat to print out the correct thing for ZRTE to pick it up WW 11/05
num :: Prsr Ptree
num = cat [list1 numchar, spaces] (\ [x1,_] cl -> Nat cl x1)

-- KT allows uppercase in alphanumq
alphanumq :: Prsr Char
alphanumq ((cfp,c,l),[]) = []
alphanumq ((cfp,c,l),ch:cs) =
	if ch >= 'a' && ch <= 'z' || ch >= '0' && ch <= '9' || ch == '_' || ch == '\'' || ch >= 'A' && ch <= 'Z'
	then [(((cfp,c+1,l),cs), ch)]
	else []

name :: Prsr String
-- really should be lexical; but not so now we had to suppress backtracking
name =
	cat [onelt alpha, list0 alphanumq, spaces] (\ [x1, x2, _] cl -> x1 ++ x2)
	`alt`
	cat [onelt (tk "_")] (\ [x1] cl -> "_")

quote :: Prsr String
quote ((cfp,c,l),[]) = []
quote ((cfp,c,l),'\"':cs) = [(((cfp,c+1,l),cs),"\"")]
quote ((cfp,c,l),ch:cs) = []

string :: Prsr Ptree
string = cat [quote, list0 anychar, quote, spaces] (\ (_:x2:_) cl -> Str cl x2)

anychar :: Prsr Char
anychar ((cfp,c,l),[]) = []
anychar ((cfp,c,l),'"':cs) = [] --"
anychar ((cfp,c,l),'\\':'"':cs) = [(((cfp,c+2,l),cs),'"')]
anychar ((cfp,c,l),'\\':'\\':cs) = [(((cfp,c+2,l),cs),'\\')]
anychar ((cfp,c,l),'\\':'n':cs) = [(((cfp,c+2,l),cs),'\n')]
anychar ((cfp,c,l),'\\':cs) = []
anychar ((cfp,c,l),'\n':cs) = [(((cfp,1,l+1),cs),'\n')]
anychar ((cfp,c,l),ch:cs) = [(((cfp,c+1,l),cs),ch)]

spaces :: Prsr String
-- redundant ... see catch-all
-- spaces ((c,l),[]) = [(((c,l),[])," ")]
spaces ((cfp,c,l),' ':cs) = spaces ((cfp,c+1,l),cs)
spaces ((cfp,c,l),'\n':cs) = spaces ((cfp,1,l+1),cs)
spaces ((cfp,c,l),'\t':cs) = spaces ((cfp,c+4,l),cs)
-- for comments
spaces ((cfp,c,l), '-':'-':cs) = untilnl cfp (c+2) l cs
-- catch-all
spaces ((cfp,c,l),cs) = [(((cfp,c,l),cs), " ")]

untilnl cfp c l [] = [(((cfp,c,l),[])," ")]
untilnl cfp c l ('\n':cs) = spaces ((cfp,1,l+1),cs)
untilnl cfp c l (_:cs) = untilnl cfp (c+1) l cs

----------------------------------------
-- Alonzo prettyprinter (Haskell translator)
----------------------------------------
{-

ppa :: Ptree -> String
ppa pt = doppa pt 0

doppa (Prog exp []) lvl = doppa exp lvl 
doppa (Prog exp dcls) lvl =
	doppa exp lvl
	++
	"\n"

-}
-----------------------------------------------------------
-- Alonzo code generator (see semantic infrastructure ZRTE.hs)
-----------------------------------------------------------

main =
	do
		(fp:modules) <- System.Environment.getArgs
		s <- readFile fp
		z2h s modules

z2h :: String -> [String] -> IO()
z2h s modules =
	do
		currDir <- System.Directory.getCurrentDirectory
		currFile <- System.Environment.getArgs
		congen (parse (concat [currDir, "/", head currFile]) toplvl s)
	where
		congen [] = error "syntax error somewhere"
		congen [((_,[]),ptree)] = cg0 modules ptree
		congen [((_,cs),ptree)] = error ("syntax error at:" ++ cs)
		congen (p:ps) = error "ambiguous parse!?"

outimp [] = putStr ""
outimp (ms:mss) =
	do
		putStr "import "
		putStrLn (map toUpper ms)
		outimp mss

ind :: Int -> IO()
ind i = indStr "" i

indStr :: String -> Int -> IO()
indStr s i =
	do
		putStrLn s
		putStr (replicate (i*2) ' ')

cg :: Int -> String -> Ptree -> IO()

--------------------------------
-- MainProg Coords Ptree [Ptree] [Ptree]		-- <some kind of expr> [@Decl] [@ModImp]
cg0 modules (MainProg cl exp ds ms) =
	do
		putStrLn "module Main (main) where"
		putStrLn "import ZRTE"
		putStrLn "import ZPOST"
		outimp modules
		mapout (cg 0 "main") "\n" ms
		putStrLn ""
		putStrLn "main :: IO()"
		putStr "main ="
		ind 1
		putStr "(print . eval) ("
		cg 2 "main" exp
		ind 1
		putStrLn ")"
		mapout (cg 0 "main") "\n" ds

-------------------------
-- ModProg Coords Ptree [Ptree] [@Ptree]			-- @Nam [@Decl] [@ModImp]
cg0 modules mp@(ModProg cl (Nam cln mcs) ds ms) =
	let
		mstr = map toUpper mcs
	in
	do
		putStrLn ("module " ++ mstr ++ " where")
		putStrLn "import ZRTE"
		putStrLn (
			if mstr == "ZPOST"
			then ""
			else "import ZPOST")
		outimp modules
		cg 0 "" mp
		

cg 0 r (ModProg cl (Nam _ _) [] _) = error ("module sans declarations! around: " ++ show cl)
cg 0 r (ModProg cl (Nam cln (mc:mcs)) ds ms) =
	do
		mapout (cg 0 r) "\n" ms
		mapout (cg 0 r) "\n" ds
cg n r (ModProg cl (Nam _ _) _ _) = error ("zoe bug: module below level 1 around: " ++ show cl)


----------------------------
-- Prog Coords Ptree [Ptree]			-- <some kind of expr> [@Decl]
cg 0 r (Prog cl exp []) = error "z2h bug: non top-level Prog with indent level 0!"
cg i r (Prog cl exp []) = cg i r exp
cg i r (Prog cl exp ds) =
	do
		ind i
		putStr "let"
		mapout (cg (i+1) r) "\n" ds
		ind i
		putStrLn "in"
		ind i
		cg i r exp

----------------------
-- ModImp Coords Ptree				-- @Nam
cg i r (ModImp clm (Nam cln mcs)) = putStrLn ("import " ++ (map toUpper mcs))

----------------------------
--  Coords [Ptree] Ptree		-- [@Nam|@Namstup] @Prog
-- Var decl for name
cg i r (Decl cld [Nam cln s] bdy) =
	do
		ind i
		putStr (s ++ " = eval")
		ind (i+1)
		putStr "("
		cg (i+2) s bdy
		ind (i+1)
		putStr ")"
		
-- Function decl. When ns empty, this is var decl. Works for PmZlist, PmTuple and some particular cases of PmZlistht
-- Var decl for PmZlistht needs special procedure which is a task for furture work.
cg i r (Decl cld (fs:ns) bdy) =
	do
		ind i
		cg i r fs
		putStr (" = eval")
		ind (i+1)
		putStr "("
		outfunc (i+2) r ns bdy
		ind (i+1)
		putStr ")"

-- WW
-- IF THEN ELSE FI code generation
cg i r (IfThenElse cl cond thenbranch elsebranch) =
	do
		ind i
		putStr ("Apply" ++ show cl)
		ind i
		putStr "("
		ind (i+1)
		putStr ("Apply" ++ show cl)
		ind (i+1)
		putStr "("
		cg (i+1) r cond
		ind (i+1)
		putStr ")"
		ind (i+1)
		putStr "("
		cg (i+1) r thenbranch
		ind (i+1)
		putStr ")"
		ind i
		putStr ")"
		ind i
		putStr "("
		cg i r elsebranch
		ind i
		putStr ")"

-- KT
cg i r (Namstup cl nts) =
	do
		ind i
		putStr ("Namstup " ++ (show cl) ++ " [")
		cgns (i+1) r nts
		ind i
		putStr "]"

	where
		cgns i r (nt1:nt2:nts) =
			do
				cgt i r nt1
				putStr ","
				cgns i r (nt2:nts)
		cgns i r [nt] =
			do
				cgt i r nt
		cgns i r [] =
			do
				putStr ""
		cgt i r (Tuple clt [(Nam cln n),v]) =
			do 
				ind i
				putStr ("Tuple " ++ (show clt) ++ " [")
				putStr ("Nam " ++ (show cln) ++ "\"" ++ n ++ "\" ,")
				cg (i+1) r v
				putStr "]"


--------------------------------------------------------
-- Implemented by WW -> Automatically generates catamorphisms for type definitions
-- FTdecl Coords Ptree [Ptree] 		-- @Nam [@NFalt]
-- NFalt Coords Ptree [Ptree] 		-- @Nam [@Fany | @Frec]
-- Fany Coords 
-- Frec Coords
cg i r ft@(FTdecl cl@(cfp,fc,fl) (Nam cln s) nfs) =
	let
		cls = cfp ++ "_" ++ show fc ++ "_" ++ show fl
		zoearg = "zoearg_" ++ cls ++ "_"
		zoeop = "zoeop_" ++ cls ++ "_"
		outerfuncdef i ((NFalt cl (Nam cln s) args), pos) =
			do
				ind i
				putStr (s ++ " = ")
				--check if non-recursive or recursive type definition
				if (null args)
					then innerfuncnonrec (i+1) cl [1 .. length nfs] [] pos
					else genouterargs (i+1) cl [1 .. length args] args [] [] pos

		-- innerfuncnonrec handles non-recursive type definitions 
		-- which just returns the function argument at the position in the TYPE definition list
		innerfuncnonrec i cl [] genops pos = 
			do
				ind i
				putStr (genops !! pos)

		innerfuncnonrec i cl (no:nos) genops pos = 
			do
				ind i
				putStr ("Func " ++ show cl)
				ind (i+1)
				putStr ("(\\ " ++ zoeop ++ show no ++ " ->")
				innerfuncnonrec (i+1) cl nos (genops ++ [(zoeop ++ show no)]) pos
				ind (i+1)
				putStr ")"

		--genouterargs outputs the argument zoe objects at the outer most part of the function
		genouterargs i cl [] args genargs argtypes pos =
			do
				genouterops (i+1) cl [1 .. length nfs] args genargs argtypes [] pos
		genouterargs i cl (na:nas) args genargs argtypes pos =
			do
				ind i
				putStr ("Func " ++ show cl)
				ind (i+1)
				--putStr ("--" ++ (show (na-1)))
				--ind (i+1)
				--putStr ("--" ++ (show (args !! (na-1))))
				--putStr ("--" ++ (show na))
				--ind (i+1)
				putStr ("(\\ " ++ zoearg ++ show na ++ " ->")
				if ("Frec" <= (show (args !! (na-1)))) 
				then
					genouterargs (i+1) cl nas args (genargs ++ [zoearg ++ show na]) (argtypes ++ ["Frec"]) pos
				else
					let
						(Fany _ t) = args !! (na-1)
						shown (Nam _ n) = n
						shown x = show x
						argtype = ("Fany" ++ "|" ++ shown t)
					in
						genouterargs (i+1) cl nas args (genargs ++ [zoearg ++ show na]) (argtypes ++ [argtype]) pos
				--outfoldg (i+1) cl nas args (genargs ++ [zoearg ++ show na]) pos
				ind (i)
				putStr ")"

		genouterops i cl [] args genargs argtypes genops pos = 
			do
				genouterapplies i cl args genargs argtypes genops pos (if (((length genargs) - 1) < 1) then 1 else ((length genargs) - 1))
		
		genouterops i cl (no:nos) args genargs argtypes genops pos =
			do
				ind i
				putStr ("Func " ++ show cl)
				ind (i+1)
				putStr ("(\\ " ++ zoeop ++ show no ++ " ->")
				genouterops (i+1) cl nos args genargs argtypes (genops ++ [(zoeop ++ show no)]) pos
				ind (i)
				putStr ")"

		-- WW Generates as many outer applies as there are (arguments-1)
		genouterapplies i cl args genargs argtypes genops pos 1 =
			do
				ind i
				putStr ("Apply" ++ show cl)
				ind i
				putStr ("(")
				geninnerfunc (i+1) cl genargs argtypes genops pos
				ind i
				putStr ")"

		genouterapplies i cl args genargs argtypes genops pos appllevels =
			do
				ind i
				putStr ("Apply" ++ show cl)
				ind i
				putStr ("(")
				genouterapplies (i+1) cl args genargs argtypes genops pos (appllevels - 1)

		--For creating inner function structure (either recursive or generic)
		geninnerfunc i cl genargs (t:tt) genops pos =
			do
				--Check that this is not a single type
				if (not (null tt))
					then do
						ind i
						putStr ("Apply" ++ show cl)
						ind i
						putStr ("(" ++ (genops !! pos))
						ind i
						putStr (")")
						ind i
						putStr ("(")
						rectypeinnerfunchelper i cl genargs (t:tt) (reverse genops) pos
					else do
						--Check whether it is a single recursive or non-recursive type
						if ("Frec" == t)
							then do
								ind (i-1)
								putStr (head genops)
								ind (i-1)
								putStr (")")
								ind (i-1)
								putStr ("(")
								rectypeinnerfunchelper (i-1) cl genargs (t:tt) (reverse genops) pos
							else do
								ind (i-1)
								putStr (genops !! pos)
								ind (i-1)
								putStr ")"
								ind (i-1)
								putStr ("(Tcheck " ++ show cl ++ (head genargs) ++ " (" ++ (scanForType t) ++ ")")
		
		rectypeinnerfunchelper i cl (g:gt) (t:tt) rgenops pos =
			do
				if ("Frec" == t)
					then do
						rectypeinnerfunc (i+1) cl g rgenops pos
					else do
						ind i
						putStr ("Tcheck " ++ show cl ++ g ++ " (" ++ (scanForType t) ++ ")")
				--Check if at the end of the list of generated arguments
				if (null gt)
					then
						putStr ("")
					else do
						ind (i)
						putStr (")")
						ind (i-1)
						putStr (")")
						ind (i-1)
						putStr ("(")
						rectypeinnerfunchelper (i-1) cl gt tt rgenops pos

		--Recursively builds inner function if it is a recursive type
		rectypeinnerfunc i cl genarg (x:xs) pos =
			do
				ind i
				putStr ("Apply" ++ show cl)
				ind i
				putStr ("(")
				rectypeinnerfunc (i+1) cl genarg (xs) pos
				ind i
				putStr (")")
				ind i
				putStr ("(" ++ x)
				ind i
				putStr (")")

		-- Innermost 
		rectypeinnerfunc i cl genarg [] pos =
			do
				putStr ("Tcheck " ++ show cl ++ " " ++ genarg ++ " " ++ s)
		
		scanForType ('|':xs) = xs
		scanForType (_:xs) = scanForType xs
		
		-- KT record the contructor name in Coords
		contls ((NFalt _ (Nam cln cn) _):xs) = (cn:(contls xs))
		contls [] = []
		buildTag (x:xs) = ("<" ++ x ++ ">" ++ (buildTag xs))
		buildTag [] = []
		tagStr = buildTag (contls nfs)
		modifyTag (x@(NFalt (s,c,l) (Nam cln cn) args):xs) = ((NFalt ((s++"<"++cn++">"),c,l) (Nam cln cn) args):(modifyTag xs))
		modifyTag [] = []
		newnfs = modifyTag nfs
	in	
	do
		--putStr ("--" ++ (show nfs))
		--ind i
		--putStr ("--" ++ (show (zip nfs [0..])))
		-- Tag each element in the nfs list with its position
		--adding nfs
		ind i
		putStr (s ++ " = " ++ "Ftype" ++ show (cfp++tagStr,fc,fl))
		ind i
		mapout (outerfuncdef i) "\n" (zip newnfs [0..])

--------------------------------------------------------------
-- Lcat Coords [Ptree]				-- [<some kid of expr>]
cg i r (Lcat cl xss) =
	let
		outlist i r [] = putStr ""
		outlist i r [xs] = cg i r xs
		outlist i r (xs1:xs2:xss) =
			do
				ind i
				cg i r xs1
				ind i
				putStr ","
				outlist i r (xs2:xss)
	in
	do
		ind i
		putStr ("Concat " ++ show cl)
		ind i
		putStr "["
		outlist (i+1) r xss
		ind i
		putStr "]"

-- KT implementation
cg i r (Nneg cl x) =
	do
		ind i
		putStr ("Neg " ++ show cl)
		ind i
		putStr "("
		ind (i+1)
		cg (i+1) r x
		ind i
		putStr ")"

-- KT implementation
cg i r (Not cl x) =
	do
		ind i
		putStr ("Not " ++ show cl)
		ind i
		putStr "("
		ind (i+1)
		cg (i+1) r x 
		ind i 
		putStr ")"
		
-- KT implementation	
cg i r (BoolOr cl xss) =
	let
		outlist i r [] = putStr ""
		outlist i r [xs] = cg i r xs
		outlist i r (xs1:xs2:xss) =
			do
				ind i
				cg i r xs1
				ind i
				putStr ","
				outlist i r (xs2:xss)
	in
	do
		ind i
		putStr ("Or " ++ show cl)
		ind i
		putStr "["
		outlist (i+1) r xss
		ind i
		putStr "]"

-- KT implementation
cg i r (BoolAnd cl xss) =
	let
		outlist i r [] = putStr ""
		outlist i r [xs] = cg i r xs
		outlist i r (xs1:xs2:xss) =
			do
				ind i
				cg i r xs1
				ind i
				putStr ","
				outlist i r (xs2:xss)
	in
	do
		ind i
		putStr ("And " ++ show cl)
		ind i
		putStr "["
		outlist (i+1) r xss
		ind i
		putStr "]"		

-- KT		
cg i r (FoldPacket cl folder@(Nam _ n1) ftype@(Nam _ n2) serverlt) =
	let
		outserverdecl i r [(x1@(Decl cld1 args1@((Nam _ s1):inputs1) body1)),(x2@(Decl cld2 args2@((Nam _ s2):inputs2) body2))] =
			do
				ind i
				putStr ("(ServerDecl" ++ show cld1 ++ "\"" ++ s1 ++ "\"")
				ind i
				putStr "("
				cg (i+1) r (Func cld1 inputs1 body1)
				putStr ")"
				ind i
				putStr "),"
				ind i
				putStr ("(ServerDecl" ++ show cld2 ++ "\"" ++ s2 ++ "\"")
				ind i
				putStr "("
				cg (i+1) r (Func cld1 inputs2 body2)
				putStr ")"
				ind i
				putStr ")"
		
		outserverdecl i r ((x1@(Decl cld args@((Nam _ s):inputs) body)):x2:xs) =
			do
				ind i
				putStr ("(ServerDecl" ++ show cld ++ "\"" ++ s ++ "\"")
				ind i
				putStr "("
				cg (i+1) r (Func cld inputs body)
				putStr ")"
				ind i
				putStr "),"
				outserverdecl i r (x2:xs)
		
		outserverdecl i r [] = putStr ""
	in
	do
		ind i
		putStr ("(FoldPacket" ++ show cl ++ " " ++ n1 ++ " " ++ n2)
		putStr " ("
		cg (i+1) r serverlt
		putStr ")"
		ind i
		putStr ")"
			
-- KT
cg i r (PmTuple cl xs) =
	do
		ind i
		putStr "("
		putStr ("Tuple " ++ "_")
		ind (i+1)
		putStr "["
		outlist (i+2) r xs
		ind (i+1)
		putStr "]"
		putStr ")"
	where
		outlist i r [] = putStr ""
		outlist i r [x] = cg i r x
		outlist i r (x1:x2:xs) =
			do
				cg i r x1;
				ind i
				putStr ","
				outlist i r (x2:xs)

-- KT
cg i r (PmZlist cl xs) =
	do
		ind i
		putStr "("
		putStr ("Listprim " ++ "_")
		ind (i+1)
		putStr "["
		outlist (i+2) r xs
		ind (i+1)
		putStr "])"
	where
		outlist i r [] = putStr ""
		outlist i r [x] =
			do
				ind i
				cg i r x
		outlist i r (x1:x2:xs) =
			do
				ind i
				cg i r x1;
				ind i
				putStr ","
				outlist i r (x2:xs)

-- KT
cg i r (PmZlistht cl xs) =
	do
		ind i
		putStr "("
		putStr ("Listprim " ++ "_")
		ind (i+1)
		putStr "("
		outlist (i+2) r xs
		ind (i+1)
		putStr ")"
		putStr ")"
	where
		outlist i r [] = putStr ""
		outlist i r [x1,x2] =
			do
				cg i r x1
				ind i
				putStr ":"
				cg i r x2
		outlist i r (x1:x2:xs) =
			do
				cg i r x1
				ind i
				putStr ":"
				outlist i r (x2:xs)

-- KT
cg i r (Pmatch cl x1@(Nam _ s) x2) =
	do
		putStr (s ++ "@")
		ind (i+1)
		putStr ("(")
		cg (i+2) r x2
		ind (i+1)
		putStr(")")
		
-- KT
cg i r (NamstupSelector cl xs) =
	let
		outSelector i r [x,(Nam cln n)] =	
			do
				putStr ("(NamstupSelector " ++ (show cl))
				putStr "("
				cg i r x
				putStr ")"
				putStr (" (Nam" ++ (show cln) ++ " \"" ++ n ++ "\"")
				putStr "))"
		outSelector i r (x1:(t@(n1:n2:_))) =
			let
				(Nam cln n) = (head . reverse) t
			in
			do
				putStr ("(NamstupSelector " ++ (show cl))
				putStr "("
				outSelector i r (x1:((reverse . tail . reverse) t))
				putStr ")"
				putStr (" (Nam" ++ (show cln) ++ " \"" ++ n ++ "\"")
				putStr "))"
	in
		outSelector i r xs

-- KT
cg i r (NamstupUpdate cl xs) =
	let
		outUpdate i r [x,y] =	
			do
				putStr ("(NamstupUpdate " ++ (show cl))
				putStr "("
				cg i r x
				putStr ")("
				cg i r y
				putStr "))"
		outUpdate i r (x1:(t@(n1:n2:_))) =
			let
				n = (head . reverse) t
			in
			do
				putStr ("(NamstupUpdate " ++ (show cl))
				putStr "("
				outUpdate i r (x1:((reverse . tail . reverse) t))
				putStr ")("
				cg i r n
				putStr "))"
	in
		outUpdate i r xs
		
--------------------------------------------------------------
-- Nadd Coords [Ptree]				-- [<some kid of expr>]
-- Added by WW 13/08/2013
cg i r (Nadd cl xss) =
	let
		outlist i r [] = putStr ""
		outlist i r [xs] = cg i r xs
		outlist i r (xs1:xs2:xss) =
			do
				ind i
				cg i r xs1
				ind i
				putStr ","
				outlist i r (xs2:xss)
	in
	do
		ind i
		putStr ("Add " ++ show cl)
		ind i
		putStr "["
		outlist (i+1) r xss
		ind i
		putStr "]"

--------------------------------------------------------------
-- Nmul Coords [Ptree]				-- [<some kid of expr>]
-- Added by WW 13/08/2013
cg i r (Nmul cl xss) =
	let
		outlist i r [] = putStr ""
		outlist i r [xs] = cg i r xs
		outlist i r (xs1:xs2:xss) =
			do
				ind i
				cg i r xs1
				ind i
				putStr ","
				outlist i r (xs2:xss)
	in
	do
		ind i
		putStr ("Mul " ++ show cl)
		ind i
		putStr "["
		outlist (i+1) r xss
		ind i
		putStr "]"
		
-- KT adding Indexing op
cg i r (IndexOp cl xss) =
	let
		outlist i r [] = putStr ""
		outlist i r [xs] = cg i r xs
		outlist i r (xs1:xs2:xss) =
			do
				ind i
				cg i r xs1
				ind i
				putStr ","
				outlist i r (xs2:xss)
	in
	do
		ind i
		putStr ("IndexOp " ++ show cl)
		ind i
		putStr "["
		outlist (i+1) r xss
		ind i
		putStr "]"

--------------------------------------------------------------
-- KT re-design comparison ops
cg i r (Equal cl x) =
	do
		putStr ("Equal " ++ show cl ++ " (")
		cg (i+1) r x
		putStr ")"

cg i r (NotEqual cl x) =
	do
		putStr ("NotEqual " ++ show cl ++ " (")
		cg (i+1) r x
		putStr ")"

cg i r (LessEqual cl x) =
	do
		putStr ("LtEqual " ++ show cl ++ " (")
		cg (i+1) r x
		putStr ")"

cg i r (Less cl x) =
	do
		putStr ("Lt " ++ show cl ++ " (")
		cg (i+1) r x
		putStr ")"

cg i r (GreaterEqual cl x) =
	do
		putStr ("GtEqual " ++ show cl ++ " (")
		cg (i+1) r x
		putStr ")"

cg i r (Greater cl x) =
	do
		putStr ("Gt " ++ show cl ++ " (")
		cg (i+1) r x
		putStr ")"

cg i r (CompareOp cl xs) =
	let
		outOp i r [x] = cg i r x
		outOp i r (x1:x2:xs) =
			do
				cg i r x1
				putStr ","
				outOp i r (x2:xs)
	in
		do
			putStr ("(CompareOp " ++ show cl ++ "[")
			outOp (i+1) r xs
			putStr "])"
		
-------------------------------------------------------------------
-- Fcomp Coords [Ptree]			-- [<some kid of expr>]; RASSOC!
cg i r (Fcomp cl xs@(x1:x2:_)) =
	let
		outappl i r cl [f,g] =
			do
				putStr ("Fcomp" ++ show cl ++ " (")
				-- ind i
				-- putStr "compose"
				-- ind i
				-- putStr "("
				cg (i+1) r f
				ind i
				putStr ") ("
				cg (i+1) r g
				ind i
				putStr ")"
		outappl i r cl (f:gs) =
			do
				putStr ("Fcomp" ++ show cl ++ " (")
				-- ind i
				-- putStr "compose"
				-- ind i
				-- putStr "("
				cg (i+1) r f
				ind i
				putStr ") ("
				outappl (i+1) r cl gs
				ind i
				putStr ")"
	in
	do
		outappl i r cl xs


-------------------------------------------------------------------
-- Tcheck Coords [Ptree]			-- [<some kid of expr>]; RASSOC!
cg i r (Tcheck cl xs@(x1:x2:_)) =
	do
		outappl i r cl xs
	where
		outappl i r cl [v,t] =
			do
				ind i
				putStr ("Tcheck" ++ show cl)
				ind i
				putStr "("
				cg i r v
				putStr ") ("
				cg i r t
				putStr ")"
		outappl i r cl (v:ts) =
			do
				ind i
				putStr ("Tcheck" ++ show cl)
				ind i
				putStr "("
				cg i r v
				ind i
				putStr ") ("
				outappl i r cl ts
				ind i
				putStr ")"

-----------------------------------------------------------------------
-- Tmap Coords [Ptree]				-- [<some kid of expr>]; RASSOC!

cg i r (Tmap cl xs@(x1:x2:_)) =
	do
		outappl i r cl xs
	where
		outappl i r cl [d, c] =
			do
				ind i
				putStr ("Mtype" ++ show cl)
				ind i
				putStr "("
				cg i r d
				ind i
				putStr ") ("
				cg i r c
				putStr ")"
		outappl i r cl (d:cs) =
			do
				ind i
				putStr ("Mtype" ++ show cl)
				ind i
				putStr "("
				cg i r d
				ind i
				putStr ") ("
				outappl i r cl cs
				putStr ")"

----------------------------------------------------------
-- Tprod Coords [Ptree]			-- [<some kid of expr>]
cg i r (Tprod cl xs) =
	do
		ind i
		putStr ("Ttype " ++ show cl)
		ind i
		putStr "["
		outlist i r xs
		putStr "]"
	where
		outlist i r [] = putStr ""
		outlist i r [x] = cg i r x
		outlist i r (x1:x2:xs) =
			do
				cg i r x1;
				ind i
				putStr ","
				outlist i r (x2:xs)

----------------------------------------------------------
-- Fappl Coords [Ptree]			-- [<some kid of expr>]
cg i r (Fappl cl xs@(x1:x2:_)) =
	do
		outappl i r cl (reverse xs)
	where
		outappl i r cl [x,f] =
			do
				-- putStrLn ""
				ind i
				-- putStrLn ("Apply" ++ show cl)
				putStr ("Apply" ++ show cl)
				ind i
				putStr "("
				cg (i+1) r f
				ind i
				putStr ")"
				ind i
				putStr "("
				cg (i+1) r x
				ind i
				putStr ")"
		outappl i r cl (x:fs) =
			do
				ind i
				putStr ("Apply" ++ show cl)
				ind i
				putStr "("
				outappl (i+1) r cl fs
				ind i
				putStr ")"
				ind i
				putStr "("
				cg (i+1) r x
				ind i
				putStr ")"

---------------------------------------------
-- Tuple Coords [Ptree]			-- [@Prog]
cg i r (Tuple cl xs) =
	do
		ind i
		putStr ("Tuple " ++ show cl)
		ind i
		putStr "["
		outlist i r xs
		putStr "]"
	where
		outlist i r [] = putStr ""
		outlist i r [x] = cg i r x
		outlist i r (x1:x2:xs) =
			do
				cg i r x1;
				ind i
				putStr ","
				outlist i r (x2:xs)

-------------------------------------------------
--Func Coords [Ptree] Ptree		-- [@Nam] Prog
cg i r (Func cl args body) =
	do
		outfunc i r args body
		-- putStrLn ""
	
---------------------------------------------
-- Listprim Coords [Ptree]			-- [@Prog]
cg i r (Listprim cl xs) =
	do
		ind i
		putStr ("Listprim " ++ show cl)
		ind i
		putStr "["
		outlist i r xs
		ind i
		putStr "]"
	where
		outlist i r [] = putStr ""
		outlist i r [x] =
			do
				ind i
				cg i r x
		outlist i r (x1:x2:xs) =
			do
				ind i
				cg i r x1;
				ind i
				putStr ","
				outlist i r (x2:xs)


--------------------
-- Nam Coords String
cg i r (Nam cl s) =
	do
		-- putStr " "
		putStr s
		-- putStr " "

--------------
-- Recn Coords 
cg i r (Recn cl) =
	do
		-- putStr " "
		putStr r
		-- putStr " "

--------------
-- Univ Coords 
cg i r (Univ cl) =
	do
		-- putStr ("Func " ++ show cl ++ "(\\ x -> Boolprim " ++ show cl ++ " True)")
		putStr "universe"

-------------------
-- Btm Coords
cg i r (Btm cl) =
	putStr ("(Error " ++ show cl ++ " (Strprim " ++ show cl ++ "\"oops - bottom!\"))")
	
-------------------
-- Error Coords Ptree
cg i r (Error cl msg) =
	do
		ind i
		putStr ("Error " ++ show cl ++ "(")
		cg (i+1) r msg
		ind i
		putStr ")"

-------------------
-- Bval Coords Bool
cg i r (Bval cl tf) = 
	putStr ("Boolprim " ++ show cl ++ show tf)

----------------
-- Funcsy Coords|Numsy Coords|Strsy Coords| Boolsy Coords|Listsy Coords|Tuplesy Coords|Ftypesy Coords|Ltypesy Coords|Mtypesy Coords|Ttypesy Coords
cg i r (Funcs cl) = putStr ("(Funcs " ++ show cl ++ ")")
cg i r (Nums cl) = putStr ("(Nums " ++ show cl ++ ")")
cg i r (Strs cl) = putStr ("(Strs " ++ show cl ++ ")")
cg i r (Bools cl) = putStr ("(Bools " ++ show cl ++ ")")
cg i r (Lists cl) = putStr ("(Lists " ++ show cl ++ ")")
cg i r (Tuples cl) = putStr ("(Tuples " ++ show cl ++ ")")
cg i r (Ftypes cl) = putStr ("(Ftypes " ++ show cl ++ ")")
cg i r (Anys cl) = putStr ("(Anys " ++ show cl ++ ")")

-- KT
cg i r (Ltypes cl x) =
	do
		ind i
		putStr ("Ltype " ++ show cl)
		ind i
		putStr "("
		cg i r x
		putStr ")"
		
cg i r (Mtypes cl) = putStr ("(Mtypes " ++ show cl ++ ")")

-- KT
cg i r (Ttypes cl xs) =
	do
		ind i
		putStr ("Ttype " ++ show cl)
		ind i
		putStr "["
		outlist i r xs
		putStr "]"
	where
		outlist i r [] = putStr ""
		outlist i r [x] = cg i r x
		outlist i r (x1:x2:xs) =
			do
				cg i r x1;
				ind i
				putStr ","
				outlist i r (x2:xs)
----------------
-- KT implementing show op to print folder type
cg i r (Shows cl x) =
	do
		putStr ("(Showop " ++ show cl)
		putStr "("
		cg i r x
		putStr "))"

{---------------
-- Tlist Coords
cg i r (Tlist cl) =
	putStr ("Func " ++ show cl ++ " (\\ x -> Ltype " ++ show cl ++ "x)")
-}

--------------
-- Emty Coords 
cg i r (Emty cl) =
	putStr " "

--------------------
-- Str Coords String
cg i r (Str cls s) =
	putStr ("Strprim " ++ show cls ++ show s)

--------------------
-- Need to put the number into the output file
cg i r (Nat cls n) =
	putStr ("Numprim " ++ show cls ++ n)

mapout out endstr xs = foldr (\ x1 x2 -> out x1 >> putStr endstr >> x2) (putStr "") xs

-- KT
outfunc i r ((Nam cl s):args) body =
			do
				ind i
				putStr ("Func " ++ show cl)
				ind i
				putStr ("(\\ " ++ s ++ " -> ")
				outfunc (i+1) r args body
				ind i
				putStr ")"
				
outfunc i r ((Pmatch cl (Nam _ n) (PmZlistht clz@(cfp,nc,nl) xs)):args) body =
	let
		cls = cfp ++ "_" ++ show nc ++ "_" ++ show nl
		targ = "tail_" ++ cls
		xsr@((y1@(Nam cl1 tn)):ys) = reverse xs
		nxsr = (Nam cl1 targ):ys
		nxs = reverse nxsr
	in
	do
		ind i
		putStr ("Func " ++ show cl)
		ind i
		putStr ("(\\ " ++ n ++ "@")
		cg i r (PmZlistht cl nxs)
		putStr (" -> ")
		ind (i+1)
		putStr ("let")
		ind (i+2)
		putStr (tn ++ " = ")
		putStr ("(Listprim " ++ show cl1 ++ " " ++ targ ++ ")")
		ind (i+1)
		putStr ("in")
		outfunc (i+2) r args body
		ind i
		putStr ")"

outfunc i r ((Pmatch cl x1 x2):args) body =
	do
		ind i
		putStr ("Func " ++ show cl)
		ind i
		putStr ("(\\ ") 
		cg i r (Pmatch cl x1 x2)
		putStr (" -> ")
		outfunc (i+1) r args body
		ind i
		putStr ")"

outfunc i r ((PmZlistht cl@(cfp,nc,nl) xs):args) body =
	let
		cls = cfp ++ "_" ++ show nc ++ "_" ++ show nl
		targ = "tail_" ++ cls
		xsr@((y1@(Nam cl1 tn)):ys) = reverse xs
		nxsr = (Nam cl1 targ):ys
		nxs = reverse nxsr
	in
	do
		ind i
		putStr ("Func " ++ show cl)
		ind i
		putStr ("(\\ ")
		cg i r (PmZlistht cl nxs)
		putStr (" -> ")
		ind (i+1)
		putStr ("let")
		ind (i+2)
		putStr (tn ++ " = ")
		putStr ("(Listprim " ++ show cl1 ++ " " ++ targ ++ ")")
		ind (i+1)
		putStr ("in")
		outfunc (i+2) r args body
		ind i
		putStr ")"
		
outfunc i r (nst@(PmTuple cln@(cfp,nc,nl) _):args) body =
	let
		cls = cfp ++ "_" ++ show nc ++ "_" ++ show nl
		zoefp = "zoefp" ++ cls
	in
	do
		ind i
		putStr ("Func " ++ show cln)
		ind i
		putStr ("(\\ " ++ zoefp ++ "@")
		cg (i+2) r nst
		putStr ("->")
		outfunc (i+2) r args body
		ind i
		putStr ")"
		
outfunc i r (nst@(PmZlist cln _):args) body =
	do	
		ind i
		putStr ("Func " ++ show cln)
		ind i
		putStr ("(\\ ") 
		cg i r nst
		putStr (" -> ")
		outfunc (i+1) r args body
		ind i
		putStr ")"
		
outfunc i r [] body =
	do
		ind i
		cg i r body

