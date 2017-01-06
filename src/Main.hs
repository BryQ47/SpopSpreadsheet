import System.IO
import Command
import Spreadsheet
import FileOperations

printError msg = putStrLn ("ERROR! " ++ msg)
printSheet sheet = putStrLn (renderSpreadsheet sheet)

initialFile = "example.csv"

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
    serializedSheet <- loadSheet initialFile
    putStrLn (show serializedSheet) -- @TODO: remove from final version of project
    let
        initialSpreadsheet = deserialize serializedSheet
    printSheet initialSpreadsheet
    mainLoop initialSpreadsheet initialFile
