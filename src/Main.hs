import System.IO
import Data.List.Split -- cabal install split

insert _ _ [] = []
insert 0 val (x:xs) = val:xs
insert ref val (x:xs) = x : (insert (ref - 1) val xs)

parseCellOp :: String -> (Maybe Int, Int)
parseCellOp [] = (Nothing, 0)
parseCellOp cmd = case (splitOn ":" cmd) of
                       [] -> (Nothing, 0)
                       (_:[]) -> (Nothing, 0)
                       (refStr:valStr:_) -> let
                                                ref = read refStr
                                                val = read valStr
                                            in (Just ref, val)
        
        

updateSpreadsheet [] _ = []
updateSpreadsheet (x:xs) cmd = case (parseCellOp cmd) of
    (Nothing,_) -> (x:xs)
    (Just ref, val) -> insert ref val (x:xs)


addCell sheet = 0:sheet

delCell [] = []
delCell (x:xs) = xs

renderSpreadsheet sheet = show sheet

mainLoop sheet = do
    putStrLn (renderSpreadsheet sheet)
    putStr "> "
    hFlush stdout
    cmd <- getLine
    case cmd of
        ":q" -> return ()
        ":add" -> mainLoop (addCell sheet)
        ":del" -> mainLoop (delCell sheet)
        _ -> do
            let
                updatedSheet = updateSpreadsheet sheet cmd
            mainLoop updatedSheet


main = mainLoop [0,0,0,0,0]
