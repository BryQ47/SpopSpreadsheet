module Cell (
    Cell(..),
    value
) where


data Cell = Cell Int

value (Cell v) = v

instance Show Cell where
    show (Cell v) = show v

instance Read Cell where
    readsPrec _ str = [(Cell (read str), [])]