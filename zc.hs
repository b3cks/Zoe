import System.Environment (getArgs)
-- import System.IO
import System.Process (system)

main =
	do
		args <- System.Environment.getArgs
		case args of
			[] -> error "no source file"
			(fn:_) -> compile fn

compile fn =
	do
		system ("zoe-core " ++ fn ++ ".zoe"  ++ " > " ++ fn ++ ".hs")
		system ("ghc " ++ fn ++ ".hs")

