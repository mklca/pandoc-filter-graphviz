{-# LANGUAGE OverloadedStrings #-}

module GraphvizBlock (graphvizBlock) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Definition
import qualified Data.Text as T

graphvizBlock :: Maybe Format -> Block -> IO Block
graphvizBlock (Just format) (CodeBlock (id, classes, keyvals) content)
    | elem "graphviz" classes =
        case format of
          Format "html" ->
              do
                (ec, out, err) <- readProcessWithExitCode "dot" ["-Tsvg", T.unpack $ "-K" <> layout] (T.unpack content)
                return $ case ec of
                           ExitSuccess -> RawBlock (Format "html")
                                 ("<div id=\"" <> id <> (T.pack $ "\" class=\"graphviz\">" <> out <> "</div>"))
                           _ -> CodeBlock (id, classes, keyvals) (T.pack err)
          Format "latex" ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot2tex" ["--figonly", T.unpack $ "--progoptions=-K" <> layout] (T.unpack content)
                return $ if length err == 0
                         then RawBlock (Format "latex") (T.pack out)
                         else CodeBlock (id, classes, keyvals) (T.pack err)
          _ ->
              do
                (ec, out, err) <- readProcessWithExitCode
                                  "dot" ["-Tpng", T.unpack $ "-K" <> layout, "-o" <> filename] (T.unpack content)
                return $ case ec of
                           ExitSuccess -> Para [Image (id, [], []) [Str content] (T.pack filename, id)]
                           _ -> CodeBlock (id, classes, keyvals) (T.pack err)
              where
                uniq = ((showDigest . sha256 . fromString) (T.unpack $ layout <> "/" <> content))
                filename = "graphviz-" <> uniq <> ".png"
    where
      layout =
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
