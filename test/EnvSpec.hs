module EnvSpec where

import Control.Exception (bracket)
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BS
import Ebnf.Prettyprinter ()
import Env (readGrammar)
import Prettyprinter (pretty)
import System.Directory (removeFile)
import System.IO.Temp (emptyTempFile)
import Test.Hspec (Spec, anyIOException, describe, it, shouldThrow)

spec_readGrammar :: Spec
spec_readGrammar =
  describe "Env.readGrammar" $ do
    it "throws IOException when the file does not exist" $
      shouldThrow (readGrammar "fooBarBazQuux") anyIOException
    it "throws IOException when the file contents cannot be read" $
      shouldThrow (withBadFile (readGrammar >=> print . pretty)) anyIOException

withBadFile :: (FilePath -> IO ()) -> IO ()
withBadFile action = bracket acquire release run
  where
    acquire :: IO FilePath
    acquire = emptyTempFile "." "temporama-bad-file-xxxx"
    release :: FilePath -> IO ()
    release = removeFile
    run :: FilePath -> IO ()
    run fp = do
      BS.writeFile fp $ BS.pack [255, 255]
      action fp
