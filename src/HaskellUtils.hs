{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for generating and outputting Haskell source.
module HaskellUtils
  ( Pragma(..)
  , writePretty
  , putPretty
  , mkModule
  ) where

import Prettyprinter
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell)

data Pragma
  = Language String
  | GhcOptions String

instance Pretty Pragma where
  pretty p =
    wrap $
    case p of
      Language str -> "LANGUAGE" <+> pretty str
      GhcOptions str -> "OPTION_GHC" <+> pretty str
    where
      wrap = enclose "{-#" "#-}"

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
runThroughPrettifier = readCreateProcessWithExitCode $ shell "hindent"

-- | Create the source for a Haskell module.
mkModule :: [Pragma] -> String -> [String] -> [String] -> Doc ann -> Doc ann
mkModule pragmas nm exports imports body =
  vcat
    [ vcat $ map pretty pragmas
    , "module" <+> pretty nm <+> exports' <+> "where"
    , vcat $ map pretty imports
    , body
    ]
  where
    exports' =
      if null exports
        then emptyDoc
        else list $ map pretty exports
