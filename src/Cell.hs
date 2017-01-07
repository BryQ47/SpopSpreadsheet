module Cell (
    CellOperation(..),
    Cell(..),
    Ref(..),
    readCell,
    parseRefString,
    refToTuple
) where

import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split

refSeparator = ","
refsSeparator = ";"

-- Address of a Cell in Spreadsheet
data Ref = Ref Int Int

refToTuple :: Ref -> (Int,Int)
refToTuple (Ref x y) = (x, y)

tupleToRef :: (Int, Int) -> Ref
tupleToRef (x,y) = (Ref x y)

instance Show Ref where
    show (Ref x y) = (show x) ++ refSeparator ++ (show y)

data CellOperation = 
    OpSum |
    OpMult |
    OpAvg

instance Show CellOperation where
    show OpSum = "Sum"
    show OpMult = "Mult"
    show OpAvg = "Avg"

data Cell =
    EmptyCell |
    StrCell String |
    NumCell Double |
    OpCell CellOperation [Ref]

instance Show Cell where
    show (EmptyCell) = ""
    show (StrCell str) = str
    show (NumCell num) = show num
    show (OpCell opType refs) = "=" ++ (show opType) ++ "(" ++ (showRefs refs) ++ ")"


readCell :: String -> Maybe Cell
readCell str = case str of
        "" -> Just EmptyCell
        '=':operation ->
            let 
                (opType,rangeStr) = case (init operation) of -- Get rid of trailing ')'
                    _:'u':'m':'(':rng -> ((Just OpSum), rng)
                    _:'u':'l':'t':'(':rng -> ((Just OpMult), rng)
                    _:'v':'g':'(':rng -> ((Just OpAvg), rng)
                    _ -> (Nothing, "")
                range = readRange rangeStr
            in case (opType, range) of
                ((Just validOp),(Just validRange)) -> Just (OpCell validOp validRange)
                _ -> Nothing
        _ -> case (tryReadDouble str) of
            Nothing -> Just (StrCell str)
            Just (number) -> Just (NumCell number)

tryReadDouble :: String -> Maybe Double
tryReadDouble str = case (reads str) of
    [(val,"")] -> Just val
    _ -> Nothing

tryReadInt :: String -> Maybe Int
tryReadInt str = case (reads str) of
    [(val,"")] -> Just val
    _ -> Nothing

parseRefString::String -> Maybe Ref
parseRefString refStr =  case (splitOn refSeparator refStr) of
    [] -> Nothing
    x1:x2:[] -> 
        let
            ref1 = tryReadInt x1
            ref2 = tryReadInt x2
        in
            case (ref1,ref2) of
                ((Just rowAddr), (Just colAddr)) -> Just (Ref rowAddr colAddr)
                _ -> Nothing
    _ -> Nothing

showRefs :: [Ref] -> String
showRefs refs = intercalate refsSeparator $ map show refs

readRange :: String -> Maybe [Ref]
readRange str = case (splitOn refsSeparator str) of
    [] -> Nothing
    (refBeg:refEnd:[]) -> let 
                       (Just x) = parseRefString refBeg
                       (Just y) = parseRefString refEnd
                       in
                            if(fst (refToTuple x) == fst (refToTuple y)) then findColRange x y
                            else findRowRange x y
    refList ->
        let
            parsedRefList = mapMaybe parseRefString refList
        in 
            if ((length refList) /= (length parsedRefList)) then
                Nothing
            else
                Just parsedRefList

-- in case of range in form e.g (1,4; 1,8) - the same row, ranges throuhout columns                
findColRange::Ref -> Ref -> Maybe [Ref]
findColRange x y = Just[ tupleToRef (a,b) | a <- [fst (refToTuple x)], b <- [snd (refToTuple x) .. snd (refToTuple y)]] 
                                                      
-- in case of range in form e.g (1,2; 5,2) - the same column, ranges throuhout rows                
findRowRange::Ref -> Ref -> Maybe [Ref]
findRowRange x y = Just [ Ref a b | a <- [fst (refToTuple x) .. fst (refToTuple y)], b <- [snd (refToTuple x)]] 