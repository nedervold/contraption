{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functionality to render and show graphs using the dot executable.
module DotUtils
  ( graphToDot
  ) where

import Algebra.Graph.Export.Dot (Style(..), export)
import Algebra.Graph.ToGraph (ToGraph(..))
import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand)
import Text.Printf (printf)

tmpdir :: FilePath
tmpdir = "./graphToDot-tmpdir/"

-- | Given the name of a graph, the graph, and a 'Style',, render the
-- graph and display it.  Uses the MacOS command \"open\".  Uses temp
-- directory \"./graphToDot-tmpdir\".
graphToDot ::
     forall g a. (ToGraph g, a ~ ToVertex g, Ord a)
  => String
  -> g
  -> Style a String
  -> IO ()
graphToDot nm gr style = do
  let src = export style gr
  openDot nm src

------------------------------------------------------------
-- | Given a graph name and its DOT-language source, render the graph
-- and display it.  Uses the MacOS command \"open\".
openDot :: String -> String -> IO ()
openDot nm src = do
  createDirectoryIfMissing False tmpdir
  writeFile (tmpdir ++ nm ++ ".dot") src
  callCommand $
    printf
      "cd %s && (cat %s.dot | dot -Tpng -o%s.png && open %s.png)"
      tmpdir
      nm
      nm
      nm
