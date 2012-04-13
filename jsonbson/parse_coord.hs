{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import qualified Data.Aeson as J
import Data.Attoparsec.Number
import Data.Bits
import qualified Data.Bson as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.CompactString.UTF8 as CS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.Vector as V

--BSON -> JSON conversion
instance J.ToJSON B.Value where
  toJSON B.Null = J.Null --add qualifiers to all value constructors
  toJSON (B.Bool b) = J.Bool b
  toJSON (B.Bin (B.Binary bstr)) = J.String (T.pack $ BS.unpack bstr)
  toJSON (B.Uuid (B.UUID id)) = J.String (T.pack $ BS.unpack id)
  toJSON (B.UserDef (B.UserDefined udef)) = J.String (T.pack $ BS.unpack udef)
  toJSON (B.ObjId id) = J.String (T.pack $ show id)
  toJSON (B.String str) = J.String (T.pack $ CS.unpack str)
  toJSON (B.Float f) = J.Number (D f)
  toJSON (B.Array arr) = J.Array (V.fromList $ map J.toJSON arr)
  toJSON (B.Fun (B.Function f)) = J.String (T.pack $ BS.unpack f)
  toJSON (B.Int32 i) = J.Number (I $ toInteger i)
  toJSON (B.Int64 i) = J.Number (I $ toInteger i)
  toJSON (B.RegEx regex) = J.String (T.pack $ show regex)
  toJSON (B.JavaScr js) = J.String (T.pack $ show js)
  toJSON (B.UTC time) = J.String (T.pack $ show time)
  toJSON (B.Sym (B.Symbol sym)) = J.String (T.pack $ CS.unpack sym)
  toJSON (B.Md5 (B.MD5 hash)) = J.String (T.pack $ BS.unpack hash)
  toJSON (B.Stamp (B.MongoStamp stamp)) = J.Number (I $ toInteger stamp)
  toJSON (B.MinMax B.MinKey) = J.String (T.pack "MinKey")
  toJSON (B.MinMax B.MaxKey) = J.String (T.pack "MaxKey")
  toJSON (B.Doc doc) = J.Object (HM.fromList $ map fieldToKV doc)
    where fieldToKV (k B.:= v) = (T.pack $ CS.unpack k, J.toJSON v)

--JSON -> BSON conversion
toBSON :: J.Value -> B.Value
toBSON J.Null = B.Null
toBSON (J.Array arr) = B.Array (map toBSON (V.toList arr))
toBSON (J.String str) = B.String (CS.pack $ T.unpack str)
toBSON (J.Number (D dbl)) = B.Float dbl
toBSON (J.Number (I int))
  | int >= (1 `shiftL` 64) = B.String (CS.pack $ show int)
  | int >= (1 `shiftL` 32) = B.Int64 (fromInteger int)
  | otherwise              = B.Int32 (fromInteger int)
toBSON (J.Bool b) = B.Bool b
toBSON (J.Object obj) = B.Doc ([(CS.pack $ T.unpack k) B.:= (toBSON v) | (k, v) <- HM.toList obj])
