module Simple (main) where

import Data.Conduit.Binary (sinkFile)
import GHC.Stats
import Network
import Network.HTTP.Conduit
import Control.Monad.Trans (liftIO)
import Data.Conduit
import Data.Int
import System.Environment


main = do
  (url:_) <- getArgs
  withSocketsDo $ do
   request <- parseUrl url
   withManager $ \m -> do
     preGC <- liftIO $ getGCStats
     
     res <- http request m
     responseBody res $$+- sinkFile "output"
     postGC <- liftIO $ getGCStats
     
     res' <- http request m
     responseBody res' $$+- sinkFile "output"
     postGC' <- liftIO $ getGCStats
     
     liftIO $ putStrLn . show $ diffGCStats preGC postGC
     liftIO $putStrLn . show $ diffGCStats postGC postGC'
     
     
     
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
     