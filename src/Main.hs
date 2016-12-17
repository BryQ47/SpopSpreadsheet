import System.IO
import Command
import Spreadsheet

printError msg = putStrLn ("ERROR! " ++ msg)
printSheet sheet = putStrLn (renderSpreadsheet sheet)

mainLoop sheet = do
    putStr "> "
    hFlush stdout
    cmdText <- getLine
    let 
        cmd = parseCommand cmdText
    case cmd of
        Quit -> return ()
        Empty -> mainLoop sheet
        BadCommand errorMsg -> do
            printError errorMsg
            mainLoop sheet
        ShowCell ref -> do
            putStrLn (showCell sheet ref)
            mainLoop sheet
        _ -> case (updateSpreadsheet sheet cmd) of
            Left errorMsg -> do
                printError errorMsg
                mainLoop sheet
            Right updatedSheet -> do 
                printSheet updatedSheet
                mainLoop updatedSheet

main = do
    printSheet initialSpreadsheet
    mainLoop initialSpreadsheet
