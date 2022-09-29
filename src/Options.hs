module Options
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

data Options = Options
  { hello :: String
  , quiet :: Bool
  , enthusiasm :: Int
  }

getOptions :: IO Options
getOptions = execParser opts

opts :: ParserInfo Options
opts =
  info
    (parser <**> helper)
    (fullDesc <> progDesc "Print a greeting for TARGET" <>
     header "contraption - a program to do something")

parser :: Parser Options
parser =
  Options <$>
  strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting") <*>
  switch (long "quiet" <> short 'q' <> help "Whether to be quiet") <*>
  option
    auto
    (long "enthusiasm" <> help "How enthusiastically to greet" <> showDefault <>
     value 1 <>
     metavar "INT")
