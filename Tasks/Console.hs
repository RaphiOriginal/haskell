module Tasks.Console
(main) where

import System.IO
import System.Exit
import Tasks.Core

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
              loop

dispatch :: String -> String -> IO ()
dispatch "list"   _  = listTasksAction taskFile
dispatch "add"    t  = addTaskAction taskFile t
dispatch "done"   nr = markDoneAction taskFile $ read nr
dispatch "rm"     nr = removeTaskAction taskFile $ read nr
dispatch "help"   _  = help
dispatch "quit"   _  = exitSuccess
dispatch _        _  = return()

listTasksAction :: FilePath -> IO ()
listTasksAction file = do
  tasks <- readTasks file
  putStrLn (concat (showTasks tasks))

help :: IO ()
help = do putStr "add a new Task: list Task\n"
          putStr "list all stored Tasks: list\n"
          putStr "mark a Task as done: done nr\n"
          putStr "remove a Task: rm nr\n"
          putStr "show help content: help\n"
          putStr "quit programm: quit\n"
