module Spreadsheet (
    Spreadsheet(..),
    createSpreadsheet,
    updateSpreadsheet,
    renderSpreadsheet,
    showCell,
    serialize,
    deserialize
) where

import Data.List
import Data.Matrix
import Data.Maybe
import Cell
import Command

-- For displaying purposes, content longer then 15 characters will be trimmed
cellWidth = 15 

------------------------ Public part ------------------------

data Spreadsheet = Spreadsheet (Matrix Cell)

createSpreadsheet :: Int -> Int -> Spreadsheet
createSpreadsheet rows cols =
    Spreadsheet $ matrix rows cols (\_ -> EmptyCell)


-- Returns either error description or updated spreadsheet
updateSpreadsheet :: Spreadsheet -> Command -> Either String Spreadsheet
updateSpreadsheet (Spreadsheet cells) cmd = 
    let
        rowsCnt = nrows cells
        colsCnt = ncols cells
        validate = validateRefs rowsCnt colsCnt
    in case cmd of
        AddCol ->
            Right (Spreadsheet (setSize EmptyCell rowsCnt (colsCnt + 1) cells))
        DelCol ->
            if colsCnt > 0 then
                Right (Spreadsheet (setSize EmptyCell rowsCnt (colsCnt - 1) cells))
            else
                Left "Spreadsheet must have at least one column"
        AddRow -> 
            Right (Spreadsheet (setSize EmptyCell (rowsCnt + 1) colsCnt cells))
        DelRow ->
            if rowsCnt > 0 then
                Right (Spreadsheet (setSize EmptyCell (rowsCnt - 1) colsCnt cells))
            else
                Left "Spreadsheet must have at least one row"
        UpdateCell ref cell ->
            if (validate [ref]) then
                case cell of
                    OpCell _ refs ->
                        if ((validateOppCell ref refs) == True ) then
                            Left "Destination cell included in range expresion" 
                        else if ((validate refs) == True) then
                            Right (Spreadsheet (setElem cell (refToTuple ref) cells))
                        else
                            Left "Cell refers to the cells outside of the spreadsheet"  
                    _ ->
                        Right (Spreadsheet (setElem cell (refToTuple ref) cells))
            else
                Left "Cell ref points outside of the spreadsheet"                                                                 


-- function for rendering spreadsheet in console, every cell can takes only 15 characters
-- next to each row and column we displays order number to provide readability                
renderSpreadsheet :: Spreadsheet -> String
renderSpreadsheet (Spreadsheet cells) = 
    let
        firstRow = '\n' : '\t' : getFristRowString 1 (ncols cells) 
        rowsSeparatingLine = '\n' : '\t' : ['-' | x <- [1..(cellWidth+1)*(ncols cells)]] ++ "\n"
        rows = toLists cells
        rowStrings = map (\row -> '\t' : '|':(intercalate "|" $ map formatCell $ map (evaluateCell cells) row)) rows
        indexedRowStrings = [firstRow] ++ (addRowsIndexes 1 rows rowStrings)
        spreadsheetString = intercalate rowsSeparatingLine indexedRowStrings
    in
        spreadsheetString

-- auxiliary method to add indexes for rows        
addRowsIndexes:: Int -> [[Cell]] -> [String] -> [String]
addRowsIndexes i [] _ = []
addRowsIndexes i(x:xs) (y:ys) = [show i ++ y] ++ (addRowsIndexes (i+1) xs ys)  

-- auxiliary method to display indexes of columns 
getFristRowString:: Int -> Int -> String
getFristRowString a nCols = if a <= nCols then show a ++ [' '| x <- [1..cellWidth]] ++ getFristRowString (a + 1) nCols
                            else []
       
-- finds in given spreadsheet indicated by reference cell and displays value
-- additionally proceed validation if reference is valid        
showCell :: Spreadsheet -> Ref -> String
showCell (Spreadsheet cells) (Ref rowId colId) = 
    if (validateRefs (nrows cells) (ncols cells) [(Ref rowId colId)]) then
        "Selected " ++ (show (Ref rowId colId)) ++ ":" ++ (show $ getElem rowId colId cells)
    else
        "Index outside of bounds of spreadsheet"

------------------------ For file operations ------------------------
serialize :: Spreadsheet -> [[String]]
serialize (Spreadsheet cells) =
    let
        cellLists = toLists cells
    in
        map (\row -> map show row) cellLists

deserialize :: [[String]] -> Spreadsheet
deserialize cellLists =
    let
        decodedCellLists = map (\row -> mapMaybe readCell row) cellLists
    in
        Spreadsheet (fromLists decodedCellLists)
    
------------------------ Private part ------------------------
-- checks if the given reference is in range of the spreadsheet
validateRefs :: Int -> Int -> [Ref] -> Bool
validateRefs rowsCnt colsCnt refs = 
    let 
        checkRefs [] = True
        checkRefs ((Ref rowId colId):otherRefs) = 
            (rowId > 0) && (rowId <= rowsCnt) && (colId > 0) && (colId <= colsCnt) && (checkRefs otherRefs)
    in
        checkRefs refs     

-- validate if the aggregate operation's range doesn't contain destination cell - to prevent infinite loop 
validateOppCell:: Ref -> [Ref] -> Bool
validateOppCell cellRef [] = False
validateOppCell cellRef (x:xs) = refToTuple cellRef == refToTuple x || validateOppCell cellRef xs
        
        
data EvaluatedCell =
    EvaluatedAsString String |
    EvaluatedAsNumber Double       

instance Show EvaluatedCell where
    show (EvaluatedAsString str) = str
    show (EvaluatedAsNumber num) = show num

cellFiller = [' ' | x <- [1..cellWidth]]
formatCell :: EvaluatedCell -> String
formatCell evaluatedCell =
    let 
        rawStr = show evaluatedCell
    in
        if (length rawStr) > cellWidth then
            (take (cellWidth - 3) rawStr) ++ "..."
        else
            take cellWidth (rawStr ++ cellFiller)

getNumValue :: EvaluatedCell -> Maybe Double
getNumValue (EvaluatedAsString _) = Nothing
getNumValue (EvaluatedAsNumber x) = Just x

safeGetElem :: Int -> Int -> Matrix Cell -> Cell
safeGetElem rowId colId cellMat =
    let
        colCnt = ncols cellMat
        rowCnt = nrows cellMat
        addrOk = validateRefs rowCnt colCnt [(Ref rowId colId)]
    in
        if addrOk then
            getElem rowId colId cellMat
        else EmptyCell
    

evaluateCell :: Matrix Cell -> Cell -> EvaluatedCell
evaluateCell cellMat cell = case cell of
    EmptyCell -> EvaluatedAsString ""
    StrCell str -> EvaluatedAsString str
    NumCell num -> EvaluatedAsNumber num
    OpCell opType refs ->
        let
            evaluatedRefs = map (\(Ref rowId colId) -> evaluateCell cellMat (safeGetElem rowId colId cellMat)) refs
            values = mapMaybe getNumValue evaluatedRefs
        in
            if ((length refs) == (length values)) then
                case opType of
                    OpSum -> EvaluatedAsNumber (sum values)
                    OpMult -> EvaluatedAsNumber (foldr (*) 1 values)
                    OpAvg -> EvaluatedAsNumber ((sum values) / (fromIntegral $ length values))
            else
                EvaluatedAsString "#bad formula"
