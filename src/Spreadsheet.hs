module Spreadsheet (
    Spreadsheet(..),
    updateSpreadsheet,
    renderSpreadsheet,
    initialSpreadsheet,
    showCell
) where

import Command

------------------------ Cell definition ------------------------ 

data Cell = Cell Int

value (Cell v) = v

instance Show Cell where
    show (Cell v) = show v

------------------------ Public part ------------------------ 

data Spreadsheet = Spreadsheet [Cell]

initialSpreadsheet = Spreadsheet [Cell 0, Cell 0, Cell 0, Cell 0]

-- Returns either error description or updated spreadsheet
updateSpreadsheet :: Spreadsheet -> Command -> Either String Spreadsheet
updateSpreadsheet (Spreadsheet cells) cmd = case cmd of
																		AddCol -> addColumn (Spreadsheet cells)
																		DelCol -> delColumn (Spreadsheet cells)
																		SumCell ref beg end -> Right (Spreadsheet (insert ref (Cell summed) cells)) 
																											where  summed = sumCells beg end cells
																		MultCell ref beg end -> Right (Spreadsheet (insert ref (Cell product) cells)) 
																											where  product = multCells beg end cells
																		AvgCell ref beg end -> Right (Spreadsheet (insert ref (Cell average) cells)) 
																											where  average = avgCells beg end cells									
																		UpdateCell ref val -> 
																			if (ref >= 0 && ref < length cells) then
																				Right (Spreadsheet (insert ref (Cell val) cells))
																			else
																				Left "Index outside of bounds of spreadsheet"

renderSpreadsheet (Spreadsheet cells) = show cells

showCell (Spreadsheet cells) ref = 
    if (ref >= 0 && ref < length cells) then
        "Selected " ++ (show ref) ++ ": " ++ (show $ cells !! ref)
    else
        "Index outside of bounds of spreadsheet"
------------------------ Private part ------------------------

insert _ _ [] = []
insert 0 val (x:xs) = val:xs
insert ref val (x:xs) = x : (insert (ref - 1) val xs)

addColumn (Spreadsheet cells) = Right (Spreadsheet (cells ++ [Cell 0])) 

delColumn (Spreadsheet []) = Left "Cannot remove column from empty spreadsheet"
delColumn (Spreadsheet cells) = Right (Spreadsheet (init cells))

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