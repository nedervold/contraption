{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functionality to render and show graphs using the dot executable.
module DotUtils
  ( graphToDot
  , openDot
  ) where

import Algebra.Graph.Export.Dot (Style(..), export)
import Algebra.Graph.ToGraph (ToGraph(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.Process (callCommand)
import Text.Printf (printf)

-- | A hacky temporary solution for a temporary directory.
hackyTmpdir :: FilePath
hackyTmpdir = "./graphToDot-tmpdir/"

-- | A temporary wrapper to somewhat hide the hack.
withTmpdir' :: (FilePath -> IO a) -> IO a
withTmpdir' action = do
  createDirectoryIfMissing False hackyTmpdir
  action hackyTmpdir

-- | Given the name of a graph, the graph, and a 'Style',, render the
-- graph and display it.  Uses the MacOS command \"open\".  Currently
-- uses a temp directory at \"./graphToDot-tmpdir\".
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
openDot nm src =
  withTmpdir' $ \tmpdir -> do
    let dotNm = nm <.> "dot"
    let imgTy = "png"
    let imgNm = nm <.> imgTy
    let cmd =
          printf
            "cd %s && (cat %s | dot -T%s -o%s && open %s)"
            tmpdir
            dotNm
            imgTy
            imgNm
            imgNm
    writeFile (tmpdir </> dotNm) src
    callCommand cmd
