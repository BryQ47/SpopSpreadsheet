module Command (
    Command(..),
    parseCommand
) where

import Data.List
import Data.List.Split
import Data.Char

data Command = 
    Quit |
    AddCol |
    DelCol |
    UpdateCell Int Int |
    BadCommand String |
    ShowCell Int |
    SumCell Int Int Int |
	MultCell Int Int Int |
	AvgCell Int Int Int |
    Empty

parseCommand :: String -> Command
parseCommand cmdText = case cmdText of
    "" -> Empty
    ":q" -> Quit
    ":addcol" -> AddCol
    ":delcol" -> DelCol
    '!':ref -> ShowCell (read ref)
    _ -> if isInfixOf "Sum" cmdText then parseComplexCommand "Sum" cmdText
			else if isInfixOf "Mult" cmdText then parseComplexCommand "Mult" cmdText
		    else if isInfixOf "Avg" cmdText then parseComplexCommand "Avg" cmdText
			else case (splitOn ":" cmdText) of
			[] -> BadCommand "Unknown command"
			(_:[]) -> BadCommand "Value not specified"
			(refStr:valStr:_) -> 
				let
					ref = read refStr
					val = read valStr
				in UpdateCell ref val

				
-- pattern for Sum / Mult / Avg command: a:Sum(b:c) a - destination b - range start c - range end
parseComplexCommand:: String -> String -> Command
parseComplexCommand complexCommand cmdText  = case (splitOn ":" cmdText) of
																							[] -> BadCommand "Unknown command"
																							(_:[]) -> BadCommand "Value not specified"
																							(refStr:indexStart:indexEnd) -> case complexCommand of 
																																		"Sum" -> if end >= beg || beg < 0 || end <0 then
																																						SumCell (read refStr :: Int) beg end 
																																						else BadCommand "Wrong range!"
																																		"Mult" -> if end >= beg || beg < 0 || end <0 then
																																						MultCell (read refStr :: Int) beg end 
																																						else BadCommand "Wrong range!"
																																		"Avg" -> if end >= beg || beg < 0 || end <0 then 
																																						AvgCell (read refStr :: Int) beg end 
																																						else BadCommand "Wrong range!"
 																																		where 
																																				beg = read ((splitOn "(" (indexStart)) !! 1) :: Int
																																				end = read ((splitOn ")" (head indexEnd)) !! 0) :: Int

