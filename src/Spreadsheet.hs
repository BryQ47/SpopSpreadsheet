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
updateSpreadsheet (Spreadsheet cells) cmd =
    case cmd of
        AddCol -> addColumn (Spreadsheet cells)
        DelCol -> delColumn (Spreadsheet cells)
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
