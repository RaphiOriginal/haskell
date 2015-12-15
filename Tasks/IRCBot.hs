-- Code from http://www.haskell.org/haskellwiki/Roll_your_own_IRC_bot

import Network
import System.IO
import Data.List
import Tasks.Core
 
server = "chat.freenode.org"
port   = 6667
chan   = "#fprog_haskell"
nick   = "Bananen_bot"
real   = "Banane bot"


--taskFile :: FilePath
--taskFile = "TasksDB.txt"

-- Main entry point. 
main :: IO ()
main = do
    h <- connectTo server (PortNumber port)      -- Connect to server
    hSetBuffering h NoBuffering                  -- Send every byte directly without buffering
    sendCmd h "NICK" nick                        -- Set the nick name of the bot
    sendCmd h "USER" (nick ++ " 0 * :" ++ real)  -- Set the real name of the bot
    sendCmd h "JOIN" chan                        -- Enter the chat room
    sendMsg h "Hi all!"                          -- Say hello to all
    listen h

-- Writes a command to the server.
sendCmd :: Handle -> String -> String -> IO ()
sendCmd h action args = do 
    hPutStr h cmd                                 -- Write cmd to server
    putStr  ("> " ++ cmd)                         -- Log cmd to the console
  where cmd = action ++ " " ++ args ++ "\r\n"

-- Sends a message to the channel.
sendMsg :: Handle -> String -> IO ()
sendMsg h msg = sendCmd h "PRIVMSG" arg
  where arg = (chan ++ " :" ++ msg)

-- Reads a command from the server.
readCmd :: Handle -> IO String
readCmd h = do
    input <- hGetLine h                            -- Read input from server   
    putStrLn ("< " ++ input)                       -- Log cmd to console
    return input

-- Listens to incoming messages and reacts correspondingly.
listen :: Handle -> IO ()
listen h = forever $ do
    cmd <- readCmd h                                 -- Read command from server                       
    if isPingCmd cmd then                            -- If it is a PING
      sendPong h cmd                                 -- Reply with a PONG
    else if isBotCmd cmd then                        -- Else if it is a command for our bot
      process h (extractNic cmd) (extractAction cmd) -- Process the command
    else return ()                                   -- Else do nothing
  where
    forever action  = action >> forever action      -- Repeat action forever

-- Returns whether the given command should be processed by our bot.
isBotCmd :: String -> Bool
isBotCmd cmd  = "!" `isPrefixOf` (extractAction cmd)

-- Returns whether the given command is a PING command.
isPingCmd :: String -> Bool
isPingCmd cmd = "PING :" `isPrefixOf` cmd

-- Writes a PONG command to the server.
sendPong :: Handle -> String -> IO ()
sendPong h cmd = sendCmd h "PONG" (':' : drop 6 cmd)

-- Extracts the nic name of a message.
-- :daniel_kroeni!~dk@nat-sta-04-084.nat.fhnw.ch PRIVMSG #fprog-i1b :!command ~> daniel_kroeni
extractNic :: String -> String
extractNic = drop 1 . takeWhile (/= '!')

-- Extracts the command part of a message.
-- :daniel_kroeni!~dk@nat-sta-04-084.nat.fhnw.ch PRIVMSG #fprog-i1b :!command ~> command
extractAction :: String -> String
extractAction = drop 1 . dropWhile (/= ':') . drop 1

-- Processes a command sent by a user.
process :: Handle -> String -> String -> IO ()
process h nic cmd = do 
    let (action:args) = words cmd    -- Split command into the first word and the rest
    case (action, unwords args) of   
         ("!reverse", args) -> sendMsg h (reverse args) -- Answer the !reverse command
         --("!list", args)    -> sendMsg h () --TODO
         ("!add", args)     -> addTaskAction taskFile args
         ("!done", args)    -> markDoneAction taskFile $ read args
         ("!rm", args)      -> removeTaskAction taskFile $ read args
         (_         , _)    -> return ()                -- Command not known - do nothing