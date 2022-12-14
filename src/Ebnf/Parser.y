{
{-# LANGUAGE ScopedTypeVariables #-}
module Ebnf.Parser(parseGrammar, testParse) where

import qualified Data.List.NonEmpty as NE
import Ebnf.Scanner
import Ebnf.Syntax
import Text.StdToken
}

%tokentype { Token }
%token COLON { $$@(Token COLON _ _) }
    ELLIPSIS { $$@(Token ELLIPSIS _ _) }
    FULL_STOP { $$@(Token FULL_STOP _ _) }
    LEFT_BRACE { $$@(Token LEFT_BRACE _ _) }
    LEFT_BRACKET { $$@(Token LEFT_BRACKET _ _) }
    LOWER_NAME { $$@(Token LOWER_NAME _ _) }
    OR { $$@(Token OR _ _) }
    RIGHT_BRACE { $$@(Token RIGHT_BRACE _ _) }
    RIGHT_BRACE_PLUS { $$@(Token RIGHT_BRACE_PLUS _ _) }
    RIGHT_BRACKET { $$@(Token RIGHT_BRACKET _ _) }
    UPPER_NAME  { $$@(Token UPPER_NAME  _ _) }
    YIELDS { $$@(Token YIELDS _ _) }

%name parseGrammar gram

%%

gram :: { Gram }
gram : prods { Gram (NE.fromList $1) }

prods :: { [Prod] }
prods : prods prod { $1 ++ [$2] }
     | prod { [$1] }

prod :: { Prod }
prod : LOWER_NAME YIELDS alts FULL_STOP { Prod $1 (NE.fromList $3) }

alts :: { [Alt] }
alts : alts OR alt { $1 ++ [$3] }
     | alt { [$1] }

alt :: { Alt  }
alt : opt_lower_name COLON terms { Alt $1 $3 }

opt_lower_name :: { Maybe Token }
opt_lower_name : LOWER_NAME { Just $1 }
    | { Nothing }

terms :: { [Term] }
terms : terms term { $1 ++ [$2] }
    | { [] }

term :: { Term }
term : vocab { VocabTerm $1 }
    | LEFT_BRACKET vocab RIGHT_BRACKET { Opt $2 }
    | LEFT_BRACE vocab RIGHT_BRACE { Rep0 $2 }
    | LEFT_BRACE vocab RIGHT_BRACE_PLUS { Rep1 $2 }
    | LEFT_BRACE vocab ELLIPSIS vocab RIGHT_BRACE { Repsep0 $2 $4 }
    | LEFT_BRACE vocab ELLIPSIS vocab RIGHT_BRACE_PLUS { Repsep1 $2 $4 }


vocab :: { Vocab }
vocab : LOWER_NAME { NT $1 }
    | UPPER_NAME { T $1 }

{
happyError :: [Token] -> a
happyError toks = error ("Parse error: " ++ show toks)

testParse :: IO ()
testParse = do
     src <- readFile "./Eiffel.ebnf"
     let toks = scan src
     let gram :: Gram = parseGrammar toks
     print gram

}



