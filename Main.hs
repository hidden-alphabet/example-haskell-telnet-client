import Network.Socket as Socket
import Control.Monad.Except
import Control.Concurrent
import System.Environment
import Control.Monad
import System.IO

type Lock = MVar ()
type TelnetSession a = IO a

sender :: Handle -> Lock -> IO ()
sender handle lock = forever $ do
  request <- getLine
  hPutStrLn handle $ request ++ "\r\n"

  yield

receiver :: Handle -> Lock -> IO ()
receiver handle lock = forever $ do
  response <- hGetContents handle
  putStrLn response 

  yield

connect :: String -> String -> TelnetSession Socket
connect host port = do
  address:_ <- getAddrInfo Nothing (Just host) (Just port)

  sock <- socket (addrFamily address) Stream defaultProtocol
  setSocketOption sock KeepAlive 1
  setSocketOption sock ReuseAddr 1

  Socket.connect sock $ addrAddress address

  return sock

main :: TelnetSession ()
main = do
  args <- getArgs

  case args of 
    [host, port] -> do 
      putStrLn $ "Connecting to telnet://" ++ host ++ ":" ++ port ++ "..."
      sock <- Main.connect host port 

      handle <- Socket.socketToHandle sock ReadWriteMode
      hSetBuffering handle LineBuffering

      lock <- newEmptyMVar

      putStrLn "Starting client..."

      forkIO $ receiver handle lock

      sender handle lock
    _ -> do 
      putStrLn "usage: telnet host port"
    
  return ()
