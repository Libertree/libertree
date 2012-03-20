module Libertree.Backend.Command where

data Command = CmdIntroduce | CmdAuthenticate | CmdNewIP | CmdUnknown deriving (Eq, Show)
