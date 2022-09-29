module Ebnf.ParserSpec where

import qualified Data.List.NonEmpty as NE
import Ebnf.Parser (parseGrammar)
import Ebnf.Prettyprinter ()
import Ebnf.Scanner (AlexPosn(..), TokenType(..), scan)
import Ebnf.Syntax
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)
import Text.StdToken (StdToken(..))

spec_parse :: Spec
spec_parse =
  describe "Ebnf.Parser.parseGrammar" $ do
    it "throws error on bad source" $
      shouldThrow
        (print $ parseGrammar $ scan "foo ::= . [] { bAr }+ ...")
        anyErrorCall
    it "succeeds on good source" $
      let goodSource = "foo ::= : ."
          toks = scan goodSource
          gram = parseGrammar toks
          fooTok = Token LOWER_NAME "foo" (AlexPn 0 1 1)
       in gram `shouldBe`
          Gram (NE.singleton $ Prod fooTok $ NE.singleton $ Alt Nothing [])
