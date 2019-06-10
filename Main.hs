import Network.Socket as Socket
import Control.Concurrent
import System.Environment
import Control.Monad
import System.IO

type TelnetSession a = IO a

sender :: Handle -> IO ()
sender handle = forever $ do
  request <- getLine
  hPutStrLn handle $ request ++ "\r\n"

receiver :: Handle -> IO ()
receiver handle = forever $ do
  response <- hGetContents handle
  putStrLn response

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

      forkIO $ receiver handle
      sender handle

    _ -> do
      putStrLn "usage: telnet host port"
   
  return ()
