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
import Data.Text
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
        Response _ res <- runResourceT $ aws conf defServiceConfig manager $ makeCommand body num
        case res of
          Data.Attempt.Success (PutObjectResponse por) -> maybe (putStrLn "no response") (\x -> putStrLn $ show x) por
          Data.Attempt.Failure e -> do
            putStrLn $ "Error" ++ show e

  forM_ [1..(read rounds :: Int)] $ run
  closeManager manager

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
