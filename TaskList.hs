import System.IO
import System.Directory

type Task = (Bool,String)
type TaskList = [Task]

taskFile :: FilePath
taskFile = "TasksDB.txt"

main :: IO ()
main = do 
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "FProg Task List Manager"
  loop

loop :: IO ()
loop = do 
    putStr "> "
    line <- getLine
    let input = words line
    if null input
      then loop
      else
        let (cmd:args) = input
        in do dispatch cmd (unwords args)
              if not (quit cmd) then loop else bye

dispatch :: String -> String -> IO ()
dispatch "list"   _  = listTasksAction taskFile
dispatch "add"    t  = addTaskAction taskFile t
dispatch "done"   nr = markDoneAction taskFile $ read nr
dispatch "rm"     nr = removeTaskAction taskFile $ read nr
dispatch "help"   _  = help
dispatch _        _  = return()

quit :: String -> Bool
quit "quit" = True
quit _ = False

bye :: IO ()
bye = putStr "Bye!\n"

help :: IO ()
help = do putStr "add a new Task: list Task\n"
          putStr "list all stored Tasks: list\n"
          putStr "mark a Task as done: done nr\n"
          putStr "remove a Task: rm nr\n"
          putStr "show help content: help\n"
          putStr "quit programm: quit\n"

readTasks :: FilePath -> IO TaskList
readTasks file = do 
    exists <- doesFileExist file
    if not exists then return []
    else do content <- readFile file
            let tasks = if null content then [] else (read content)
            length tasks `seq` return tasks

writeTasks :: FilePath -> TaskList -> IO ()
writeTasks file tasks = writeFile file (show tasks)

listTasksAction :: FilePath -> IO ()
listTasksAction file = do
  tasks <- readTasks file
  putStrLn (concat (showTasks tasks))

modifyTasks :: FilePath -> (TaskList -> TaskList) -> IO ()
modifyTasks file f = do
  tasks <- readTasks file
  writeTasks file $ f tasks

addTaskAction :: FilePath -> String -> IO ()
addTaskAction file desc =
  modifyTasks file $ addTask desc

markDoneAction :: FilePath -> Int -> IO ()
markDoneAction file nr =
  modifyTasks file $ markDone nr

removeTaskAction :: FilePath -> Int -> IO ()
removeTaskAction file nr = modifyTasks file $ removeTask nr

addTask :: String -> TaskList -> TaskList
addTask task tasks = tasks ++ [(False, task)]

--numberTasks :: TaskList -> [(Int,Task)]
--numberTasks tasks = zip [0..] tasks

numberTasks :: TaskList -> [(Int,Task)]
numberTasks tl = number tl 0 
   where number [] _ = []
         number (t:ts) n = (n,t) : number ts (n+1)

removeTask :: Int -> TaskList -> TaskList
removeTask nr tasks = take nr tasks ++ drop (nr+1) tasks 

{-
markDone :: Int -> TaskList -> TaskList
markDone tId tasks = 
  map (\(nr, (done,desc)) -> if nr == tId then (True,desc) else (done,desc)) (numberTasks tasks)
-}

markDone :: Int -> TaskList -> TaskList
markDone nr tasks =
  let (prefix, (_, desc) : rest) = splitAt nr tasks
  in prefix ++ (True, desc) : rest                  

showTasks :: TaskList -> [String]
showTasks [] = ["Horray! You've got nothing to do."]
showTasks l  = map format $ numberTasks l       
  where format (nr,(done,desc)) = nrStr nr ++ " " ++ doneStr done ++ " " ++ desc ++ "\n"
        doneStr True  = "[X]"
        doneStr False = "[ ]"    
        nrStr nr =  show nr ++ "."   
