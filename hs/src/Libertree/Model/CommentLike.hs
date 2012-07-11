{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Libertree.Model.CommentLike where

import Libertree.Model.Common

import qualified Database.PostgreSQL.Simple as Db
import Data.Int (Int64)
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)

data CommentLikeM = CommentLikeM
                      Int64         -- member id
                      Int64         -- comment id
                      (Maybe Int64) -- remote id

instance Model CommentLikeM where
  create db entity@(CommentLikeM m c r) =
    Db.withTransaction db $ do
      Db.execute db
        "INSERT INTO comment_likes (member_id, comment_id, remote_id) VALUES (?, ?, ?)"
          (m, c, r)
      distribute db entity

instance Distributable CommentLikeM where
  isLocal (CommentLikeM _ _ Nothing) = True
  isLocal _ = False
  distribute db e =
    if isLocal e
      then do
        [x] :: [Db.Only Int64] <- Db.query_ db "SELECT currval('comment_likes_id_seq')"
        enqueue db
          "distribution"
          "Request::COMMENT-LIKE"
          (encode $ object [ "comment_like_id" .= Db.fromOnly x ])
      else return 0
