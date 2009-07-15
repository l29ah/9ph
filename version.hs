module Main where

import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString.Lazy
import Data.Int
import Data.Binary
import Data.Binary.Builder
import Debug.Trace
import Nine hiding (main)

next s (Rversion _ _ _ _ _ _) = do
  let tattachMessPayload = encode tattachClientMessage
  putStrLn . show $ C.length tattachMessPayload
  putStrLn $ printHex tattachMessPayload
  send s (encode tattachClientMessage) >>= print . ("Sent: " ++) . show
  print "Tattach message sent, getting response"
  msg <- recv s 100
  putStrLn $ printHex msg
  return msg

finalstep (Rattach _ _ _ q) = do
  putStrLn "Rattach came back.. QID following"
  print q

finalstep (Rerror _ _ _ _ message) = do
  putStrLn "Rerror returned"
  print message


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
          print (decode msg :: MessageClient)
          nmsg <- next sock (message (decode msg :: MessageClient))
          putStrLn . show $ C.length msg
          putStrLn $ printHex msg
          finalstep (message (decode nmsg :: MessageClient))
          sClose sock
          print "closed socket"
