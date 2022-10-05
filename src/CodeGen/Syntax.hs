-- | Generate code defining syntax for the language.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.Syntax
  ( mkSyntaxSrc
  ) where

import Config.ModuleName (toImport)
import Config.SyntaxType (SyntaxType(..))
import Control.Monad.State
import Data.Char (toLower, toUpper)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Ebnf.Syntax
import Env (Env(..))
import HaskellUtils (Import(..), Pragma(..), bracedGroup, mkData, mkModule)
import Names (constructorName, typeName)
import Prettyprinter
import Text.Printf (printf)
import Text.StdToken (_tokenText)

-- | Create a 'Doc' for the module  defining syntax for the language.
mkSyntaxSrc :: Env -> Doc ann
mkSyntaxSrc Env {..} =
  mkModule pragmas (pretty envSyntaxModuleName) exports imports (vcat body)
  where
    body =
      if envSyntaxType == RecordLensSyntax
        then syntax ++ map mkTH ps'
        else syntax
    mkTH (Prod hd _) =
      prettyStr $ printf "makeLenses ''%s" (typeName $ _tokenText hd)
      where
        prettyStr :: String -> Doc ann
        prettyStr = pretty
    syntax = map mkSyntax ps'
    pragmas = [Language "DeriveDataTypeable", Language "TemplateHaskell"]
    exports =
      if envSyntaxType == RecordLensSyntax
        then typeExports ++ lensExports
        else typeExports
      where
        typeExports = map mkExport $ S.toList envGramNonterminals
    imports =
      [ Import "Data.Data(Data)"
      , Qualified "Ebnf.Extensions" "Ext"
      , Import "Control.Lens.TH"
      , toImport envTokenModuleName
      ]
    Gram ps = envGrammar
    ps' = NE.toList ps
    mkExport :: String -> String
    mkExport nt = printf "%s(..)" $ typeName nt
    mkSyntax (Prod hd alts) =
      mkData
        (typeName nm)
        (map (mkRhs envSyntaxType nm) $ NE.toList alts)
        (S.toList envDatatypeDerivations)
      where
        nm = _tokenText hd
    lensExports =
      concat
        [ mkTermNames False ctorNm ts
        | Prod hd alts <- ps'
        , let prodNm = _tokenText hd
        , Alt mCtor ts <- NE.toList alts
        , let ctorNm =
                case mCtor of
                  Just tok -> _tokenText tok
                  Nothing -> prodNm
        ]

mkRhs :: SyntaxType -> String -> Alt -> Doc ann
mkRhs synTy prodNm (Alt mCtor ts) =
  case synTy of
    SimpleSyntax -> hsep (pretty nm : map mkTerm ts)
    RecordSyntax -> pretty nm <+> mkRecordTerms False ctorNm ts
    RecordLensSyntax -> pretty nm <+> mkRecordTerms True ctorNm ts
  where
    nm = constructorName ctorNm
    ctorNm :: String
    ctorNm =
      case mCtor of
        Just tok -> _tokenText tok
        Nothing -> prodNm

mkRecordTerms :: Bool -> String -> [Term] -> Doc ann
mkRecordTerms lensed ctorNm ts = bracedGroup comma $ zipWith f nms ts
  where
    nms = mkTermNames lensed ctorNm ts
    f nm t = hsep [pretty nm, "::", mkTerm t]

mkTermNames :: Bool -> String -> [Term] -> [String]
mkTermNames lensed ctorNm ts = numbered ts'
  where
    ts' :: [String]
    ts' = map (mkTermName lensed ctorNm) ts

type MSI = M.Map String Int

type S = State MSI

numbered :: [String] -> [String]
numbered ss = map f ss'
  where
    f :: (String, Int) -> String
    f (s, i) =
      if m' M.! s == 1
        then s
        else s ++ show i
    ss' :: [(String, Int)]
    m' :: MSI
    (ss', m') = runState (mapM count ss) M.empty
    count :: String -> S (String, Int)
    count s = do
      m <- get
      let cnt =
            case M.lookup s m of
              Just n -> n + 1
              Nothing -> 1
      put $ M.insert s cnt m
      pure (s, cnt)

mkTermName :: Bool -> String -> Term -> String
mkTermName lensed ctorNm t = f $ concat (map toLower p : map cap ps'')
  where
    f =
      if lensed
        then ('_' :)
        else id
    cap [] = []
    cap (c:cs) = toUpper c : map toLower cs
    p:ps'' = ps' ++ ps
    ps' = splitOn "_" ctorNm
    ps =
      case t of
        VocabTerm v -> mkVocabName v
        Opt b -> "opt" : mkVocabName b
        Rep0 b -> "rep0" : mkVocabName b
        Rep1 b -> "rep1" : mkVocabName b
        Repsep0 b s -> "repsep0" : mkVocabName b ++ mkVocabName s
        Repsep1 b s -> "repsep1" : mkVocabName b ++ mkVocabName s

mkVocabName :: Vocab -> [String]
mkVocabName (NT tok) = splitOn "_" $ _tokenText tok
mkVocabName (T tok) = splitOn "_" (_tokenText tok) ++ ["token"]

mkTerm :: Term -> Doc ann
mkTerm (VocabTerm v) = mkVocab v
mkTerm (Opt b) = parens ("Ext.Opt" <+> mkVocab b)
mkTerm (Rep0 b) = parens ("Ext.Rep0" <+> mkVocab b)
mkTerm (Rep1 b) = parens ("Ext.Rep1" <+> mkVocab b)
mkTerm (Repsep0 b s) = parens $ hsep ["Ext.Repsep0", mkVocab s, mkVocab b]
mkTerm (Repsep1 b s) = parens $ hsep ["Ext.Repsep1", mkVocab s, mkVocab b]

mkVocab :: Vocab -> Doc ann
mkVocab (NT tok) = pretty $ typeName $ _tokenText tok
mkVocab (T _tok) = "Token"
