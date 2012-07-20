{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Libertree.Model.PostLike where

import Libertree.Model.Common

import qualified Database.PostgreSQL.Simple as Db
import Data.Int (Int64)
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)

data PostLikeM = PostLikeM
                   Int64         -- member id
                   Int64         -- post id
                   (Maybe Int64) -- remote id

instance Model PostLikeM where
  create db entity@(PostLikeM m p r) =
    Db.withTransaction db $ do
      Db.execute db
        "INSERT INTO post_likes (member_id, post_id, remote_id) VALUES (?, ?, ?)"
          (m, p, r)
      distribute db entity

instance Distributable PostLikeM where
  isLocal (PostLikeM _ _ Nothing) = True
  isLocal _ = False
  distribute db e =
    if isLocal e
      then do
        [x] :: [Db.Only Int64] <- Db.query_ db "SELECT currval('post_likes_id_seq')"
        enqueue db
          "distribution"
          "Request::POST-LIKE"
          (encode $ object [ "post_like_id" .= Db.fromOnly x ])
      else return 0
