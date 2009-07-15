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
             | Tattach {size :: Word32,
                        mtype :: Word8,
                        tag :: Word16,
                        fid :: Word32,
                        afid :: Word32,
                        unames :: Word16,
                        uname :: ByteString,
                        anames :: Word16,
                        aname :: ByteString}
             | Rattach {size :: Word32,
                        mtype :: Word8,
                        tag :: Word16,
                        qid :: ByteString}
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

mtTattach :: Word8
mtTattach = 104

mtRattach :: Word8
mtRattach = 105

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
          putMessage (Tattach s mt t f af unl un anl an) = do
                                putWord32le s
                                putWord8 mt
                                putWord16le t
                                putWord32le f
                                putWord32le af
                                putWord16le unl
                                putLazyByteString un
                                putWord16le anl
                                putLazyByteString an      

    get = do s <- getWord32le
             mtype <- getWord8
             getSpecific s mtype
        where 
          getSpecific s mtRversion = rtVersion mtRversion s
          getSpecific s mtTversion = rtVersion mtTversion s
          getSpecific s mtTattach = do 
                                t <- getWord16le
                                f <- getWord32le
                                af <- getWord32le
                                unl <- getWord16le
                                un <- getLazyByteString $ fromIntegral unl
                                anl <- getWord16le
                                an <- getLazyByteString $ fromIntegral anl
                                return $ MessageClient $ Tattach {size=s,
                                                                  mtype=mtTattach,
                                                                  tag=t,
                                                                  fid=f,
                                                                  afid=af,
                                                                  unames=unl,
                                                                  uname=un,
                                                                  anames=anl,
                                                                  aname=an}
          getSpecific s mtRattach = do
                                t <- getWord16le
                                q <- getLazyByteString $ fromIntegral 13
                                return $ MessageClient $ Rattach {size=s,
                                                                  mtype=mtRattach,
                                                                  tag=t,
                                                                  qid=q}
                                
          getSpecific s mtRerror = do t <- getWord16le
                                      ss <- getWord16le
                                      e <- getLazyByteString $ fromIntegral ss
                                      return $ MessageClient $ Rerror {size=s,
                                                                       mtype=mtRerror,
                                                                       tag=t,
                                                                       ssize=ss,
                                                                       ename=e}
          rtVersion val s = do t <- getWord16le
                               ms <- getWord32le
                               ss <- getWord16le
                               v <- getLazyByteString $ fromIntegral ss
                               return $ MessageClient $ Rversion {size=s,
                                                                  mtype=val,
                                                                  tag=t,
                                                                  msize=ms,
                                                                  ssize=ss,
                                                                  version=v}
tversionClientMessage :: MessageClient
tversionClientMessage = MessageClient $ Tversion {size = 19,
                                                  mtype = mtTversion,
                                                  tag = 65535,
                                                  msize = 1024,
                                                  ssize = 6,
                                                  version = pack "9P2000"}

-- 19 + 2 strings
tattachClientMessage :: MessageClient
tattachClientMessage = MessageClient $ Tattach {size = 21,
                                                mtype = mtTattach,
                                                tag = 10,
                                                fid = 999,
                                                afid = -1,
                                                unames = 1,
                                                uname = pack "9",
                                                anames = 1,
                                                aname = pack "p"}
printHex :: ByteString -> String
printHex b = concatMap (printf "%02x") $ unpack b

main :: IO ()
main = do putStrLn $ "encoded client Tversion: " ++ printHex (encode tversionClientMessage)
          print tversionClientMessage
