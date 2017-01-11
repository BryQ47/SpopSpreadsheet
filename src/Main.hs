import System.IO
import Command
import Spreadsheet
import FileOperations

printError msg = putStrLn ("ERROR! " ++ msg)
printSheet sheet = putStrLn (renderSpreadsheet sheet)

initialFile = "newSheet.csv"

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
        CreateNew rows cols -> do
            let
                newSheet = createSpreadsheet rows cols
            printSheet newSheet
            mainLoop newSheet currentFile
        OpenFile filename -> do
            putStrLn ("Opening file " ++ filename)
            serializedSheet <- loadSheet filename
            let
                newSheet = deserialize serializedSheet
            printSheet newSheet
            mainLoop newSheet filename
        WriteFile Nothing -> do
            putStrLn ("Writing to file " ++ currentFile)            
            saveSheet currentFile (serialize sheet)
            mainLoop sheet currentFile
        WriteFile (Just filename) -> do
            putStrLn ("Wrtiting to file " ++ filename)
            saveSheet filename (serialize sheet)
            mainLoop sheet filename
        _ -> case (updateSpreadsheet sheet cmd) of
            Left errorMsg -> do
                printError errorMsg
                mainLoop sheet currentFile
            Right updatedSheet -> do 
                printSheet updatedSheet
                mainLoop updatedSheet currentFile

main = do
    let
        initialSpreadsheet = createSpreadsheet 5 5
    printSheet initialSpreadsheet
    mainLoop initialSpreadsheet initialFile
