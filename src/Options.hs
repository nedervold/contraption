-- | Command-line options for contraption.
module Options
  ( Options(..)
  , Product(..)
  , getOptions
  ) where

import qualified Data.Set as S
import Options.Applicative
import Text.Printf (printf)

-- | Products the user may request.
data Product
  = EbnfGrammar -- ^ the prettyprinted EBNF source for the grammar
  | DependencyGraph -- ^ an image of the dependency graph
  deriving (Eq, Ord, Show)

readProductOpt :: String -> Either String Product
readProductOpt "ebnf-grammar" = Right EbnfGrammar
readProductOpt "dependency-graph" = Right DependencyGraph
readProductOpt opt = Left $ printf "Cannot parse %s as a product." (show opt)

-- | Command-line options for contraption.
data Options = Options
  { grammarFile :: FilePath -- ^ source location for the grammar's EBNF file
  , outputProducts :: S.Set Product -- ^ set of 'Product's requested
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
   some (argument (eitherReader readProductOpt) (metavar "PRODUCTS...")))
    {- <*>
  switch (long "quiet" <> short 'q' <> help "Whether to be quiet") <*>
  option
    auto
    (long "enthusiasm" <> help "How enthusiastically to greet" <> showDefault <>
     value 1 <>
     metavar "INT")
-}
