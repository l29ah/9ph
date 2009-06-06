module Nine where

import Prelude hiding(init)
import Text.Printf
import Data.ByteString.Lazy.Char8 hiding (concatMap, putStrLn)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Debug.Trace

data Message = Tversion {size :: Word32, 
                         mtype :: Word8,
                         tag :: Word16, 
                         msize :: Word32,
                         ssize :: Word16,
                         version :: ByteString}
             | Rversion {size :: Word32,
                         mtype :: Word8,
                         tag :: Word16, 
                         msize :: Word32, 
                         ssize :: Word16,
                         version :: ByteString}
             | Rerror {size :: Word32, 
                       mtype :: Word8,
                       tag :: Word16, 
                       ssize :: Word16,
                       ename :: ByteString}
               deriving (Read,Show)


mtTversion :: Word8
mtTversion = 100

mtRversion :: Word8
mtRversion = 101

mtRerror :: Word8
mtRerror = 107

newtype MessageClient = MessageClient { message :: Message}
    deriving (Read,Show)


instance Binary MessageClient where
    put m = do let m9 = message m
               putMessage m9
        where 
          putMessage (Tversion s mt t ms ss v) = do putWord32le s 
                                                    putWord8 mt
                                                    putWord16le t
                                                    putWord32le ms
                                                    putWord16le ss
                                                    putLazyByteString v
      

    get = do s <- getWord32le
             mtype <- getWord8
             getSpecific s mtype
        where 
          getSpecific s mt 
                      | mt == mtRversion = do t <- getWord16le
                                              ms <- getWord32le
                                              ss <- getWord16le
                                              v <- getLazyByteString $ fromIntegral ss
                                              return $ MessageClient $ Rversion {size=s,
                                                                                 mtype=mt,
                                                                                 tag=t,
                                                                                 msize=ms,
                                                                                 ssize=ss,
                                                                                 version=v}
                      | mt == mtRerror = do t <- getWord16le
                                            ss <- getWord16le
                                            e <- getLazyByteString $ fromIntegral ss
                                            return $ MessageClient $ Rerror {size=s,
                                                                             mtype=mt,
                                                                             tag=t,
                                                                             ssize=ss,
                                                                             ename=e}

tversionClientMessage :: MessageClient
tversionClientMessage = MessageClient $ Tversion {size = 19,
                                                  mtype = mtTversion,
                                                  tag = 65535,
                                                  msize = 1024,
                                                  ssize = 6,
                                                  version = pack "9P2000"}

printHex :: ByteString -> String
printHex b = concatMap (printf "%02x") $ unpack b

main :: IO ()
main = do putStrLn $ "encoded client Tversion: " ++ printHex (encode tversionClientMessage)
          print tversionClientMessage
