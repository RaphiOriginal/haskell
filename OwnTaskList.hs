import System.IO
import System.Directory

type Task = (Bool, String)
type TaskList = [Task]

-- a)

readTasks :: IO TaskList
readTasks = do content <- readFile "TaskDataBase.yolo"
               let tasks = if null content then [] else (read content)
               length tasks `seq` return tasks

writeTasks :: TaskList -> IO ()
writeTasks t = writeFile "TaskDataBase.yolo" (show t)

-- b)

addTask :: String -> TaskList -> TaskList
addTask t tl = (False,t) : tl

-- c)

numberTasks :: TaskList -> [(Int, Task)]
numberTasks tl = numberTasksSupport 0 tl
    where numberTasksSupport _ [] = []
          numberTasksSupport n tl = (n, last tl) : numberTasksSupport (n + 1) (init tl)

-- d)

markDone :: Int -> TaskList -> TaskList
markDone _ [] = []
markDone 0 ((_,t):tl) = (True, t) : markDone (0-1) tl
markDone n (t:tl) = t : markDone (n-1) tl

-- e)

main :: IO ()
main = do taskcommand <- getLine
		  let task = words taskcommand
          checkTask (head task) (unwords $ tail task)
          main

checkTask :: String -> String -> IO ()
checkTask "list" _ = listAction
checkTask "add" t = addAction t
checkTask "done" nr = doneAction $ read nr
checkTask _ _ = return()

listAction :: TaskList -> IO ()
listAction tl = do show $ numberTasks $ readTasks

addAction :: String -> IO ()
addAction s = do modifyTasks $ addTask s

doneAction :: Int -> IO ()
doneAction nr = do modifyTasks $ markDone nr

modifyTasks :: (TaskList -> TaskList) -> IO ()
modifyTasks f = do tasks <- readTasks
                   writeTasks $ f tasks