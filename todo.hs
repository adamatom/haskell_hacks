import System.Environment
import System.Directory
import System.IO
import Data.List

add :: [String] -> IO ()
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")

view :: [String] -> IO ()
view [filename] = do
    contents <- readFile filename
    let todotasks = lines contents
        numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todotasks
    putStr $ unlines numberTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

help :: [String] -> IO ()
help _= do
    putStrLn "usage:"
    putStrLn "    todo add [file] [item] - Add an item to the todo list"
    putStrLn "    todo view [file] - prints the todo list in a numbered list"
    putStrLn "    todo remove [file] - remove an entry from a file"

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           , ("help", help)
           ]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
