{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Control.Monad

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
