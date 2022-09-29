module Env
  ( Env(..)
  , mkEnv
  ) where

import Options

data Env = Env
  { envOptions :: Options
    -- add more here
  }

mkEnv :: Options -> IO Env
mkEnv options = pure $ Env {envOptions = options}
