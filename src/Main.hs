import System.IO
import Command
import Spreadsheet

printError msg = putStrLn ("ERROR! " ++ msg)
printSheet sheet = putStrLn (renderSpreadsheet sheet)

initialFile = "data/example.csv"

mainLoop sheet currentFile = do
    putStr "> "
    hFlush stdout
    cmdText <- getLine
    let 
        cmd = parseCommand cmdText
    case cmd of
        Quit -> return ()
        Empty -> mainLoop sheet currentFile
        BadCommand errorMsg -> do
            printError errorMsg
            mainLoop sheet currentFile
        ShowCell ref -> do
            putStrLn (showCell sheet ref)
            mainLoop sheet currentFile
        OpenFile filename -> do
            putStrLn ("Opening file " ++ filename)
            -- TODO: Load spreadsheet from file
            mainLoop sheet filename
        WriteFile Nothing -> do
            putStrLn ("Writing to file " ++ currentFile)
            -- TODO: Save spreadsheet to file
            mainLoop sheet currentFile
        WriteFile (Just filename) -> do
            putStrLn ("Wrtiting to file " ++ filename)
            -- TODO: Save spreadsheet to file
            mainLoop sheet filename
        _ -> case (updateSpreadsheet sheet cmd) of
            Left errorMsg -> do
                printError errorMsg
                mainLoop sheet currentFile
            Right updatedSheet -> do 
                printSheet updatedSheet
                mainLoop updatedSheet currentFile

main = do
    printSheet initialSpreadsheet
    mainLoop initialSpreadsheet initialFile
