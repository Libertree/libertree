{-# LANGUAGE OverloadedStrings #-}

module Libertree.Backend.Dispatch where

import Libertree.Backend.Command
import qualified Data.ByteString.Char8 as BS

-- TODO: need to use State monad to figure out what the previous request was
-- TODO: spawn IO processes
buildResponse :: Command -> BS.ByteString
buildResponse cmd =
  BS.pack $
    case cmd of
      CmdIntroduce -> "Hello there"
      CmdAuthenticate -> "Authenticate request"
      CmdNewIP -> "New IP request"
      CmdUnknown -> "Command invalid"

