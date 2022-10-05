{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for generating and outputting Haskell source.
module HaskellUtils
  ( writePretty
  , putPretty
  -- * helper types
  , Import(..)
  , Pragma(..)
  -- * Haskell syntax
  , mkModule
  , mkData
  , mkNewtype
  , mkDefn
  , mkFuncTy
  , bracedGroup
  ) where

import Data.List (intersperse, sort)
import Data.Ord (comparing)
import qualified Data.Set as S
import Prettyprinter
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell)
import Text.Printf (printf)

-- | Haskell pragmas
data Pragma
  = Language String
  | GhcOptions String
  deriving (Eq, Ord)

instance Pretty Pragma where
  pretty p =
    wrap $
    case p of
      Language str -> "LANGUAGE" <+> pretty str
      GhcOptions str -> "OPTION_GHC" <+> pretty str
    where
      wrap d = hsep ["{-#", d, "#-}"]

-- | Prettify and write Haskell source out to a file.  If prettification
-- fails, write the raw argument string.
writePretty :: FilePath -> String -> IO ()
writePretty fp src = do
  (ec, src', _stderr) <- runThroughPrettifier src
  let outp =
        case ec of
          ExitSuccess -> src'
          _ -> src
  writeFile fp outp

-- | Prettify and put Haskell source out to stdout.  If prettification
-- fails, put the raw argument string.
putPretty :: String -> IO ()
putPretty src = do
  (ec, src', _stderr) <- runThroughPrettifier src
  let outp =
        case ec of
          ExitSuccess -> src'
          _ -> src
  putStrLn outp

-- | Run the raw source through a Haskell prettifier.
runThroughPrettifier :: String -> IO (ExitCode, String, String)
-- TODO I'd like the prettifier to be configurable.  Also I need to
-- check that it exists.
runThroughPrettifier = readCreateProcessWithExitCode $ shell "hindent"

data Import
  = Import String
  | Qualified String
              String
  deriving (Eq)

instance Ord Import where
  compare = comparing f
    where
      f (Import str) = (str, "")
      f (Qualified str asStr) = (str, asStr)

instance Show Import where
  show (Import str) = printf "import %s" str
  show (Qualified str asStr) = printf "import qualified %s as %s" str asStr

instance Pretty Import where
  pretty = pretty . show

-- | Source for a Haskell module.
mkModule ::
     S.Set Pragma -> Doc ann -> [String] -> S.Set Import -> Doc ann -> Doc ann
mkModule pragmas nm exports imports body =
  vcat
    [ vcat $ map pretty $ S.toList pragmas
    , "module" <+> nm <+> exports' <+> "where"
    , vcat $ map pretty $ S.toList imports
    , body
    ]
  where
    exports' =
      if null exports
        then emptyDoc
        else tupled $ map pretty exports

-- | Source for a Haskell data declaration.
mkData :: String -> [Doc ann] -> [String] -> Doc ann
mkData nm rhss derivations =
  "data" <+>
  pretty nm <+>
  align
    (vcat
       (zipWith (<+>) seps rhss ++
        ["deriving" <+> tupled (map pretty $ sort derivations)]))
  where
    seps = "=" : repeat "|"

-- | Source for a Haskell newtype declaration.
mkNewtype :: String -> Doc ann -> [String] -> Doc ann
mkNewtype nm rhs derivations =
  hsep
    [ "newtype"
    , pretty nm
    , align $
      vcat ["=" <+> rhs, "deriving" <+> tupled (map pretty $ sort derivations)]
    ]

-- | Source for a Haskell definition.
mkDefn :: String -> Doc ann -> Doc ann -> Doc ann
mkDefn nm ty rhs = vcat [sig, defn]
  where
    nm' = pretty nm
    sig = hsep [nm', "::", ty]
    defn = hsep [nm', "=", align rhs]

-- | Source for a Haskell function type.
mkFuncTy :: [Doc ann] -> Doc ann
mkFuncTy ds = hsep $ intersperse "->" ds

bracedGroup :: Doc ann -> [Doc ann] -> Doc ann
bracedGroup sep' ds = braces (align $ vsep $ punctuate sep' ds)
