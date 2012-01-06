{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.Jerry.SuperCollider.Server
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (at least generalized newtype deriving)
--
-- Connecting with the sc_server...
--
--------------------------------------------------------------------------------


module Sound.Jerry.SuperCollider.Server
  (

    ScServerLoc(..)
  , default_sc_server

  , ScHandle            -- opaque, naturally

  , scOpen
  , scClose
  , scSendTo
  , scRecvFrom
        
  ) where

import Sound.Jerry.OSC                          -- package: jerry-osc

import Network.BSD
import Network.Socket hiding ( sendTo, recvFrom )
import Network.Socket.ByteString

import qualified Data.ByteString as S

-- | Connection details of Supercollider server.
--
data ScServerLoc = ScServerLoc
      { sc_svr_hostname :: String
      , sc_svr_portnum  :: PortNumber
      }

-- | Default connection details:
--
-- > ip address: "127.0.0.1" 
--
-- > port number: 57110
--
default_sc_server :: ScServerLoc
default_sc_server = ScServerLoc { sc_svr_hostname = "127.0.0.1" 
                                , sc_svr_portnum  = 57110
                                }



data ScHandle = ScHandle
      { sc_socket       :: Socket
      , sc_sock_addr    :: SockAddr
      }


-- | Implementation note - going straight to binding the socket
-- is perhaps a bit rushed...
--
scOpen :: ScServerLoc -> IO ScHandle
scOpen (ScServerLoc { sc_svr_hostname = hostname
                    , sc_svr_portnum  = portnum }) = do 
    { sckt <- socket AF_INET Datagram 0
    ; server <- inet_addr hostname
    ; let saddr = SockAddrInet portnum server
    ; return $ ScHandle { sc_socket  = sckt
                        , sc_sock_addr = saddr
                        }
    }



-- | Note - this wraps @sendTo@ from Network, thus it inherits 
-- all @sendTo@\'s problems.
--
scSendTo :: ScHandle -> Packet -> IO Int
scSendTo h p = sendTo (sc_socket h) (serializePacket p) (sc_sock_addr h)


scRecvFrom :: ScHandle -> IO (S.ByteString, SockAddr)
scRecvFrom h = fmap post $ recvFrom (sc_socket h) 4096
  where 
    post (p,addr) = (p,addr) -- (deserializePacket p,  addr)

-- | Close the socket to the sc_server.
--
scClose :: ScHandle -> IO ()
scClose = sClose . sc_socket


