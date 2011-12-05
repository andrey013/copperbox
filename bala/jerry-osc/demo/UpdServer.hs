
module UpdServer where



import Control.Concurrent
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import Data.Word
import IO
import Network.Socket
import Network.BSD


udpServer :: Int -> (String -> IO ()) -> IO ()
udpServer portnum handlerfn = withSocketsDo $ do 
    (server_addr:_) <- getAddrInfo Nothing Nothing (Just (show portnum))
    sock <- socket (addrFamily server_addr) Datagram defaultProtocol
    bindSocket sock (addrAddress server_addr)
    procMessages sock
  where
    procMessages sock = do
      (msg,_,_) <- recvFrom sock 1024
      handlerfn msg
      procMessages sock

main = udpServer 9001 print
