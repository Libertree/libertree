{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Libertree.Model.Post where

import Libertree.Model.Common

import qualified Database.PostgreSQL.Simple as Db
import Data.Int (Int64)
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)

data PostM = PostM
               Int64         -- member id
               (Maybe Int64) -- remote id
               Bool          -- public
               Text          -- text

instance Model PostM where
  create db entity@(PostM m r p t) =
    Db.withTransaction db $ do
      Db.execute db
        "INSERT INTO posts (member_id, remote_id, public, text) VALUES (?,?,?,?)"
          (m, r, p, t)
      distribute db entity

instance Distributable PostM where
  isLocal (PostM _ Nothing _ _) = True
  isLocal _ = False
  distribute db e =
    if isLocal e
      then do
        [x] :: [Db.Only Int64] <- Db.query_ db "SELECT currval('posts_id_seq')"
        enqueue db
          "distribution"
          "Request::POST"
          (encode $ object [ "post_id" .= Db.fromOnly x ])
      else return 0
