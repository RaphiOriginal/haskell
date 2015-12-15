module Tasks.Core where

import System.Directory

taskFile :: FilePath
taskFile = "TasksDB.txt"

type Task = (Bool,String)
type TaskList = [Task]

readTasks :: FilePath -> IO TaskList
readTasks file = do 
    exists <- doesFileExist file
    if not exists then return []
    else do content <- readFile file
            let tasks = if null content then [] else (read content)
            length tasks `seq` return tasks

writeTasks :: FilePath -> TaskList -> IO ()
writeTasks file tasks = writeFile file (show tasks)

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