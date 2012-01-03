
module DemoSc where

import Sound.Jerry.SuperCollider.Server

import Sound.Jerry.OSC.Datatypes
import Sound.Jerry.OSC.ReadOSC
import Sound.Jerry.OSC.WriteOSC


import Control.Concurrent
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S
import Data.Word
import IO
import Network.BSD
import Network.Socket hiding ( sendTo )
import Network.Socket.ByteString

import Data.Char


-- Clearly we want to implement a sc client!

test_msg :: [Word8]
test_msg = [47,116,101,115,116,0,0,0,44,105,105,105,0,0,0,0,0,0,0,1,0,0,0,2,0,0,0,3]

printMsg :: [Word8] -> IO ()
printMsg = putStrLn . map (chr . fromIntegral)


demo0 :: IO ()
demo0 = printMsg test_msg

demo01 = runGet getPacket (L.pack test_msg)

demo02 = let a = runGet getPacket (L.pack test_msg) 
         in printMsg $ S.unpack $ serializePacket a

demo02a = let a = runGet getPacket (L.pack test_msg) 
         in print $ S.unpack $ serializePacket a


demo03 = main2 "127.0.0.1" 57110 $ 
    [ Message "/status" [] 
    ]

main2 :: String -> PortNumber -> [Packet] -> IO ()
main2 hostname portnum pks = withSocketsDo $ do 
    sckt <- socket AF_INET Datagram 0
    server <- inet_addr hostname
    let sa = SockAddrInet portnum server
    let send1 a = threadDelay 10000 >> sendTo sckt (serializePacket a) sa 
    mapM send1 pks 
    sClose sckt


scQuit = main2 "127.0.0.1" 57110 $ 
    [ Message "/quit" [] 
    ]


main3 = withSocketsDo $ do 
   h <- scOpen default_sc_server
   scSend h [ Message "/quit" [] ] 
   scClose h