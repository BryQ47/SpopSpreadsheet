module Command (
    Command(..),
    parseCommand
) where

import Data.List.Split
import Data.Char
import Cell

data Command = 
        Quit |
        AddCol |
        DelCol |
        AddRow |
        DelRow |
        UpdateCell Ref Cell |
        BadCommand String |
        ShowCell Ref |
        Empty |
        WriteFile (Maybe String) |
        OpenFile String |
        CreateNew Int Int

parseCommand :: String -> Command
parseCommand cmdText = case cmdText of
    "" -> Empty
    ':':controlCmd -> case controlCmd of
                "q" -> Quit
                "addrow" -> AddRow
                "delrow" -> DelRow                
                "addcol" -> AddCol
                "delcol" -> DelCol
                'n':dimensions -> case (splitOn " " $ trim dimensions) of
                        [] -> BadCommand "Missing dimensions of a new spreadsheet"
                        (_:[]) -> BadCommand "Missing height of a new spreadsheet"
                        (w:h:[]) -> case ((tryReadInt w), (tryReadInt h)) of
                                ((Just width), (Just height)) -> CreateNew width height
                                _ -> BadCommand "Incorrect dimensions format for a new spreadsheet"
                        _ -> BadCommand "Too many parameters of a new spreadsheet given"
                'o':file -> case (readFilename file) of
                        Nothing -> BadCommand "Filename is required"
                        Just filename -> OpenFile filename
                'w':file -> case (readFilename file) of
                        Nothing -> WriteFile Nothing
                        Just filename -> WriteFile (Just filename)
                _ -> BadCommand "Unknown control command"
    '!':ref -> case (parseRefString ref) of
        (Just validRef) -> ShowCell validRef
        Nothing -> BadCommand "Invalid cell ref of cell to show"
    _ -> case (splitOn ":" cmdText) of
        [] -> BadCommand "Unknown command - missing ref and cell value part"
        ref:[] -> case (parseRefString ref) of
                (Just validRef) -> UpdateCell validRef EmptyCell
                Nothing -> BadCommand "Invalid cell ref or wrong command"
        ref:cell:[] -> case (parseRefString ref) of
                (Just validRef) -> case (readCell cell) of
                        Nothing -> BadCommand "Invalid cell value"
                        (Just validCell) -> UpdateCell validRef validCell
                Nothing -> BadCommand "Invalid cell ref"
        _ -> BadCommand "Unknown command - misformulated ref and/or cell value part"

readFilename :: String -> Maybe String
readFilename text = case (trim text) of
        [] -> Nothing
        filename -> Just filename

trim :: String -> String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs where
        dropSpaceTail maybeStuff "" = ""
        dropSpaceTail maybeStuff (x:xs)
                | isSpace x = dropSpaceTail (x:maybeStuff) xs
                | null maybeStuff = x : dropSpaceTail "" xs
                | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs
