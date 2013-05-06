{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Aws
import Aws.S3
import Blaze.ByteString.Builder.Word
import Control.Monad
import Control.Monad.Trans
import Data.Attempt
import Data.Conduit
import Data.Functor
import Data.Int
import Data.Text hiding (lines)
import GHC.Stats
import Network.HTTP.Conduit
import System.Environment
import System.Random

main :: IO ()
main = do
  (rounds:bytes:_) <- getArgs
  let numBytes = read bytes
  Just cred <- loadCredentialsFromEnv
  manager <- newManager def
  rawBytes <- liftIO $ fromWord8s <$> replicateM numBytes (randomRIO (0,255))
  let conf = Configuration Timestamp cred $ defaultLog Warning
      body = RequestBodyBuilder (fromIntegral numBytes) rawBytes

  let run num = do
        preGCStats <- getGCStats
        Response _ res <- runResourceT $ aws conf defServiceConfig manager $ makeCommand body num
        postGCStats <- getGCStats
        case res of
          Data.Attempt.Success (PutObjectResponse por) -> maybe (putStrLn "no response") (\x -> putStrLn $ show x) por
          Data.Attempt.Failure e -> do
            putStrLn $ "Error" ++ show e
        return (num, diffGCStats preGCStats postGCStats)

  results <- mapM run [1..(read rounds::Int)]
  closeManager manager

  mapM_ (putStrLn . show) results

makeCommand body num = PutObject { poObjectName         = pack $ "data/" ++ show num
                                 , poBucket             = "capture-benchmark"
                                 , poContentType        = Nothing
                                 , poCacheControl       = Nothing
                                 , poContentDisposition = Nothing
                                 , poContentEncoding    = Nothing
                                 , poContentMD5         = Nothing
                                 , poExpires            = Nothing
                                 , poAcl                = Just AclPrivate
                                 , poStorageClass       = Nothing
                                 , poRequestBody        = body
                                 , poMetadata           = []
                                 }

diffGCStats :: GCStats -> GCStats -> GCStatsIncremental
diffGCStats old new = GCStatsIncremental
    { ibytesAllocated      = (bytesAllocated new) - (bytesAllocated old)
    , inumGcs              = (numGcs new) - (numGcs old)
    , inumByteUsageSamples = (numByteUsageSamples new) - (numByteUsageSamples old)
    , ibytesUsed           = (cumulativeBytesUsed new) - (cumulativeBytesUsed old)
    , ibytesCopied         = (bytesCopied new) - (bytesCopied old)
    , imutatorCpuSeconds   = (mutatorCpuSeconds new) - (mutatorCpuSeconds old) -- ?
    , imutatorWallSeconds  = (mutatorWallSeconds new) - (mutatorWallSeconds old)
    , igcCpuSeconds        = (gcCpuSeconds new) - (gcCpuSeconds old)
    , igcWallSeconds       = (gcWallSeconds new) - (gcWallSeconds old)
    , icpuSeconds          = (cpuSeconds new) - (cpuSeconds old)
    , iwallSeconds         = (wallSeconds new) - (wallSeconds old)
    }

data GCStatsIncremental = GCStatsIncremental
      { ibytesAllocated      :: !Int64
      , inumGcs              :: !Int64
      , inumByteUsageSamples :: !Int64
      , ibytesUsed           :: !Int64
      , ibytesCopied         :: !Int64
      , imutatorCpuSeconds   :: !Double
      , imutatorWallSeconds  :: !Double
      , igcCpuSeconds        :: !Double
      , igcWallSeconds       :: !Double
      , icpuSeconds          :: !Double
      , iwallSeconds         :: !Double
      } deriving (Show)
