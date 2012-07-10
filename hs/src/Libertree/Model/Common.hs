{-# LANGUAGE OverloadedStrings, StandaloneDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}

module Libertree.Model.Common where

import qualified Database.PostgreSQL.Simple as Db
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Control.Applicative
import Data.Typeable (Typeable)
import Data.IP
import Data.Int (Int64)

instance FromField IP where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just a) = pure $ read (unpack a)

instance ToField IP where
  toField = Escape . pack . show

deriving instance Typeable IP

class Model a where
  create :: Db.Connection -> a -> IO Int64
  --update
  --delete

class (Model a) => Distributable a where
  isLocal :: a -> Bool
  distribute :: Db.Connection -> a -> IO Int64

enqueue :: Db.Connection -> String -> String -> ByteString -> IO Int64
enqueue db q t p = Db.execute db
  "INSERT INTO jobs (queue, task, params) VALUES (?,?,?)" ( q, t, p )
