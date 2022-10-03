-- | Command-line options for contraption.
module Options
  ( Options(..)
  , getOptions
  ) where

import qualified Data.Set as S
import Options.Applicative
import Product (Product(..), readProductOpt)

-- | Command-line options for contraption.
data Options = Options
  { grammarFile :: FilePath -- ^ source location for the grammar's EBNF file
  , outputProducts :: S.Set Product -- ^ set of 'Product's requested
  , build :: Bool
  , inPlace :: Bool
  }

-- | Extract the options from the command line.
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
     help "Source for the grammar") <*>
  (S.fromList <$>
   some (argument (eitherReader readProductOpt) (metavar "PRODUCTS..."))) <*>
  switch (short 'b' <> long "build" <> help "Build the generated source") <*>
  switch (short 'i' <> long "in-place" <> help "Prettyprint inputs in-place")
