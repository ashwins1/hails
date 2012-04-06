{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.Bson as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.CompactString.UTF8 as CS
import Control.Applicative
import Control.Monad
import qualified Data.Text as T

instance ToJSON B.Value where
  toJSON Null = Null
  toJSON (Bool b) = Bool b
  toJSON (String str) = String (T.pack $ CS.unpack str)
  toJSON (Float f) = Number f
  toJSON (Array a) = Array (fromList $ map toJSON a)
  toJSON (Fun (Function f)) = String (pack $ unpack f)
  toJSON (Int32 i) = Number (toInteger i)
  toJSON (Int64 i) = Number (toInteger i)
  toJSON (UTC time) = String (BS.pack $ show time)
  toJSON (Symbol sym) = String (T.pack $ CS.unpack str)
  toJSON (MD5 hash) = String (T.pack $ BS.unpack hash)
  

{-
data Coord = Coord { x :: Double, y :: Double } deriving Show

instance FromJSON Coord where
   parseJSON (Object v) = Coord    <$>
                          v .: "x" <*>
                          v .: "y"

-- A non-Object value is of the wrong type, so use mzero to fail.
   parseJSON _          = mzero

main = do json <- B.readFile "coord.json"
          let c = decode json :: Maybe Coord
          case c of
            Nothing -> putStrLn "oh no!"
            Just coord -> putStrLn (show coord)
-}
