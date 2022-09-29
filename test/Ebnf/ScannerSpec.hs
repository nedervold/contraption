module Ebnf.ScannerSpec where

import Ebnf.Prettyprinter ()
import Ebnf.Scanner (TokenType(..), scan)
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)
import Text.StdToken (StdToken(..))

spec_scan :: Spec
spec_scan =
  describe "Ebnf.Scanner.scan" $ do
    it "throws error on bad source" $
      shouldThrow (print $ scan "!@#$%^&*()") anyErrorCall
    it "succeeds on good source" $
      let goodSource = "foo ::= : ."
          toks = scan goodSource
       in do map _tokenType toks `shouldBe`
               [LOWER_NAME, YIELDS, COLON, FULL_STOP]
             unwords (map _tokenText toks) `shouldBe` goodSource
