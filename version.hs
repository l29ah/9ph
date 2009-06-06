module Main where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Int
import Data.Binary
import Data.Binary.Builder
import Debug.Trace
import Nine hiding (main)

tversion :: Int8
tversion = 100


main :: IO ()
main = withSocketsDo $
       do 
          ainfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "6872")
          let a = head ainfo
          sock <- socket AF_INET Stream defaultProtocol
          connect sock (addrAddress (traceShow a a))
          let tversionMessPayload = encode tversionClientMessage
          putStrLn . show $ C.length tversionMessPayload
          putStrLn $ printHex tversionMessPayload
          send sock (tversionMessPayload) >>= print . ("Sent: " ++) . show
          print "Tversion Message sent, receiving response"
          msg <- recv sock 50
          putStrLn $ printHex msg
          print "Received Message"
          sClose sock
          print "closed socket, decoding message"
          print (decode msg :: MessageClient)
