module Main where

import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Int
import Data.Binary hiding (get, put)
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.Builder
import Debug.Trace
import NineP

main :: IO ()
main = withSocketsDo $
       do 
          ainfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6872")
          let a = head ainfo
          sock <- socket AF_INET Stream defaultProtocol
          putStrLn "Trying to connect"
          connect sock (addrAddress (traceShow a a))
          putStrLn "connected!"
          let version = Msg TTversion (-1) $ Tversion 1024 "9P2000"
          putStrLn $ "About to send: " ++ show version
          send sock $ runPut (put version) 
          msg <- recv sock 50
          putStrLn $ show $ msg 

            
--           let tversionMessPayload = encode tversionClientMessage
--           putStrLn . show $ C.length tversionMessPayload
--           putStrLn $ printHex tversionMessPayload
--           send sock (tversionMessPayload) >>= print . ("Sent: " ++) . show
--           print "Tversion Message sent, receiving response"
--           msg <- recv sock 50
--           putStrLn $ printHex msg
--           print "Received Message"
--           print (decode msg :: MessageClient)
--           nmsg <- next sock (message (decode msg :: MessageClient))
--           putStrLn . show $ C.length msg
--           putStrLn $ printHex msg
--           finalstep (message (decode nmsg :: MessageClient))
--           sClose sock
--           print "closed socket"
