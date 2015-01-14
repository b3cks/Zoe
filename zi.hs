import System.Environment (getArgs)
import System.IO
import System.Process (system)
import System.Exit

main =
	do
		args <- System.Environment.getArgs
		case args of
			[] -> interloop "zoeinteraction" False [] ""
			["-d"] -> interloop "zoeinteraction" True [] ""
			("-d":modules) -> interloop "zoeinteraction" True modules ""
			(modules) -> interloop "zoeinteraction" False modules ""

interloop fn debug modules lastCommand =
	do
    putStr "Zoe:> "
    hFlush stdout
    x:xs <- getLine
--STARTTW--
    -- Make lines starting with ':' command lines for zi TW
    if x==':'
    then
      case xs of
        "quit"    -> exitSuccess
        "q"   -> exitSuccess
        "help"    -> putStrLn "Commands are ':quit', ':debug', ':last' and ':help'."
        "debug"   -> interloop fn (not debug) modules ""
        -- Allows the recall of the last (non-command) line for evaluation
        "last"   -> do 
        	putStr("Zoe:> "++lastCommand)
        	hFlush stdout 
        	interloop' fn debug modules lastCommand lastCommand
        "l"   -> do 
        	putStr("Zoe:> "++lastCommand)
        	hFlush stdout 
        	interloop' fn debug modules lastCommand lastCommand
        otherwise -> putStrLn "Unrecognised Command, try ':help'." else
      	interloop' fn debug modules (x:xs) "" 
    interloop fn debug modules lastCommand
--ENDTW--

interloop' fn debug modules s lastCommand =
	do
		h <- openFile (fn ++ ".zoe") WriteMode
		hPutStrLn h s
		hClose h
		system ("zoe-core " ++ fn ++ ".zoe " ++ outm modules ++ " > " ++ fn ++ ".hs")
		if debug
		then
			do
				putStrLn "-------------"
				putStrLn "HASKELL CODE:"
				hFlush stdout
				system ("type " ++ fn ++ ".hs")
				putStrLn "--------------"
				putStrLn "ZOE EXECUTION:"
				hFlush stdout
				system ("runghc " ++ fn)
		else
			do
				system ("runghc " ++ fn)
		interloop fn debug modules s

outm ms = foldr (\x y -> x ++ " " ++ y) "" ms