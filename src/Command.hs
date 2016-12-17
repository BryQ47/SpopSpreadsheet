module Command (
    Command(..),
    parseCommand
) where

import Data.List.Split

data Command = 
    Quit |
    AddCol |
    DelCol |
    UpdateCell Int Int |
    BadCommand String |
    ShowCell Int |
    Empty

parseCommand :: String -> Command
parseCommand cmdText = case cmdText of
    "" -> Empty
    ":q" -> Quit
    ":addcol" -> AddCol
    ":delcol" -> DelCol
    '!':ref -> ShowCell (read ref)
    _ -> case (splitOn ":" cmdText) of
        [] -> BadCommand "Unknown command"
        (_:[]) -> BadCommand "Value not specified"
        (refStr:valStr:_) -> 
            let
                ref = read refStr
                val = read valStr
            in UpdateCell ref val


