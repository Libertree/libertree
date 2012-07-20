{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Libertree.Model.Comment where

import Libertree.Model.Common

import qualified Database.PostgreSQL.Simple as Db
import Data.Int (Int64)
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)

data CommentM = CommentM
                  Int64         -- member id
                  Int64         -- post id
                  (Maybe Int64) -- remote id
                  Text          -- text

instance Model CommentM where
  create db entity@(CommentM m p r t) =
    Db.withTransaction db $ do
      Db.execute db
        "INSERT INTO comments (member_id, post_id, remote_id, text) VALUES (?,?,?)"
          (m, p, r, t)
      distribute db entity

instance Distributable CommentM where
  isLocal (CommentM _ _ Nothing _) = True
  isLocal _ = False
  distribute db e =
    if isLocal e
      then do
        [x] :: [Db.Only Int64] <- Db.query_ db "SELECT currval('comments_id_seq')"
        enqueue db
          "distribution"
          "Request::COMMENT"
          (encode $ object [ "post_id" .= Db.fromOnly x ])
      else return 0
