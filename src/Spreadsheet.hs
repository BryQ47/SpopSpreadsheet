module Spreadsheet (
    Spreadsheet(..),
    updateSpreadsheet,
    renderSpreadsheet,
    showCell,
    serialize,
    deserialize
) where

import Cell
import Command

data Spreadsheet = Spreadsheet [[Cell]]

-- Returns either error description or updated spreadsheet
updateSpreadsheet :: Spreadsheet -> Command -> Either String Spreadsheet
updateSpreadsheet (Spreadsheet cells) cmd = case cmd of
        AddCol -> addColumn (Spreadsheet cells)
        DelCol -> delColumn (Spreadsheet cells)
        AddRow -> addRow (Spreadsheet cells)
        DelRow -> delRow (Spreadsheet cells)
        UpdateCell ref val ->  Right (Spreadsheet (insertToSpreadsheet ref (Cell val) cells))
    
     {-   if ( fst ref >= 0 && ref < length cells) then
            Right (Spreadsheet (insert ref (Cell val) cells))
        else
            Left "Index outside of bounds of spreadsheet"
            -}
  {-  SumCell ref beg end -> Right (Spreadsheet (insert ref (Cell summed) cells)) 
                                        where  summed = sumCells beg end cells
    MultCell ref beg end -> Right (Spreadsheet (insert ref (Cell product) cells)) 
                                        where  product = multCells beg end cells
    AvgCell ref beg end -> Right (Spreadsheet (insert ref (Cell average) cells)) 
                                        where  average = avgCells beg end cells                                                                        
   
-}
renderSpreadsheet (Spreadsheet cells) = show cells

showCell (Spreadsheet cells) ref = 
    if (ref >= 0 && ref < length cells) then
        "Selected " ++ (show ref) ++ ": " ++ (show $ cells !! ref)
    else
        "Index outside of bounds of spreadsheet"

------------------------ For file operations ------------------------
serialize :: Spreadsheet -> [[String]]
serialize (Spreadsheet cells) =
    let
        encodedCells = map show cells
        
    in [encodedCells, encodedCells, encodedCells]

deserialize :: [[String]] -> Spreadsheet
deserialize cells = Spreadsheet (parseStringContent cells)

parseStringContent [] = []
parseStringContent (x:xs) = [map read x] ++ parseStringContent xs 
    
------------------------ Private part ------------------------
insertToSpreadsheet:: (Int, Int) -> Cell -> [[Cell]] ->[[Cell]]
insertToSpreadsheet _ _ [[]] = [[]]
insertToSpreadsheet _ _ []= []
insertToSpreadsheet (0,b) cell (x:xs) = [(insert b cell x)] ++ xs
insertToSpreadsheet (a,b) cell (x:xs) =  [x] ++ insertToSpreadsheet (a-1, b) cell xs

insert::Int -> Cell -> [Cell] -> [Cell]
insert _ _ [] = []
insert 0 val (x:xs) = val:xs
insert ref val (x:xs) = x : (insert (ref - 1) val xs)

-- find proper row in the spreadsheet matrix
findRow _ [[]] = []
findRow 0 (x:xs) = x
findRow a (x:xs) = findRow (a-1) xs

addRow (Spreadsheet []) = Right (Spreadsheet [[Cell 0]])
addRow (Spreadsheet cells) = Right (Spreadsheet (cells ++ [getRow (length (head cells))]))

delRow (Spreadsheet []) = Left "Cannot remove row from empty spreadsheet"
delRow (Spreadsheet cells) =  Right (Spreadsheet (take x cells))
                                            where x = length cells - 1

getRow:: Int -> [Cell]
getRow 0 = []
getRow x = [Cell 0] ++ getRow (x-1)


addColumn (Spreadsheet cells) = Right (Spreadsheet (newMatrix cells))
                                                    where
                                                    newMatrix [[]] = [[Cell 0]]
                                                    newMatrix [] = []
                                                    newMatrix (x:xs) = [x ++ [(Cell 0)]] ++ newMatrix xs 

                                                    
delColumn (Spreadsheet []) = Left "Cannot remove column from empty spreadsheet"
delColumn (Spreadsheet cells) = Right (Spreadsheet (newMatrix cells))
                                                    where 
                                                    newMatrix [[]] = [[]]
                                                    newMatrix [] = []
                                                    newMatrix (x:xs) = [take ((length x) - 1) x] ++ newMatrix xs

newMatrix2 [[]] = [[]]
newMatrix2 [] = []
newMatrix2 (x:xs) = [take ((length x) - 1) x] ++ newMatrix2 xs                                                
                                                    
{-
-- Computes sum of cells values from range  
sumCells:: Int -> Int -> [Cell] -> Int
sumCells _ _ [] = error("Cannot proceed sum operation - empty spreadsheet")
sumCells 0 0 (x:xs) = value x
sumCells 0 end (x:xs) = value x + sumCells 0 (end - 1) xs
sumCells beg end list = if beg <= l || end <= l 
                                                                        then sumCells (beg -1) end list
                                                                        else error("Cannot count sum, wrong range") 
                                                                        where l = length list

-- Computes product of cells from range
multCells:: Int -> Int -> [Cell] -> Int
multCells _ _ [] = error("Cannot proceed sum operation - empty spreadsheet")
multCells 0 0 (x:xs) = value x
multCells 0 end (x:xs) = value x * multCells 0 (end - 1) xs
multCells beg end list = if  beg <= l || end <= l 
                                                                        then multCells (beg -1) end list
                                                                        else error("Cannot count product, wrong range") 
                                                                        where l = length list
                                                                        
-- Computes average fof cells from range
avgCells:: Int -> Int -> [Cell] -> Int
avgCells beg end list = if beg <= l || end <= l 
                                                                    then (sumCells beg end list) `div` (end - beg + 1)
                                                                    else error("Cannot count average, wrong range") 
                                                                    where l = length list
                                                                    -}