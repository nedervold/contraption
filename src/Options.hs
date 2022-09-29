module Options
  ( Options(..)
  , getOptions
  ) where

import Options.Applicative

newtype Options = Options
  { grammarFile :: String
  }

getOptions :: IO Options
getOptions = execParser opts

opts :: ParserInfo Options
opts =
  info
    (parser <**> helper)
    (fullDesc <> progDesc "Generate parser and prettyprinter code" <>
     header "contraption - a program to generate parsers and prettyprinters")

parser :: Parser Options
parser =
  Options <$>
  strOption
    (short 'g' <> long "grammar" <> metavar "GRAMMAR-FILE" <>
     help "Source for the grammar")
    {- <*>
  switch (long "quiet" <> short 'q' <> help "Whether to be quiet") <*>
  option
    auto
    (long "enthusiasm" <> help "How enthusiastically to greet" <> showDefault <>
     value 1 <>
     metavar "INT")
-}
