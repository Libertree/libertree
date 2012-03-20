{-# LANGUAGE OverloadedStrings #-}

module Libertree.Backend.Request (parseRawRequest) where

import Libertree.Backend.Command
import qualified Data.ByteString.Char8 as BS

-- TODO: also build payload JSON
parseRawRequest :: BS.ByteString -> Command
parseRawRequest req =
  let (cmd, payload) = splitRawRequest req in
    case cmd of
      Just "INTRODUCE"    -> CmdIntroduce
      Just "AUTHENTICATE" -> CmdAuthenticate
      Just "NEW-IP"       -> CmdNewIP
      Just _              -> CmdUnknown
      Nothing             -> CmdUnknown

splitRawRequest :: BS.ByteString -> (Maybe String, Maybe [String])
splitRawRequest req = do
  let parts = words $ BS.unpack req
  if null parts then (Nothing, Nothing)
	            else do
                  let cmd = head parts
                      payload = toJSON $ init parts in
                     (Just cmd, Just payload)

-- TODO: noop
toJSON = id

