{-# LANGUAGE OverloadedStrings #-}

module GraphvizBlock (graphvizBlock) where

import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Pandoc.Definition
import qualified Data.Text as T

graphvizBlock :: Maybe Format -> Block -> IO Block
graphvizBlock (Just format) (CodeBlock (identifier, classes, keyvals) content)
    | elem "graphviz" classes =
        case format of
          Format "html" ->
              do
                (ec, out, err) <- readProcessWithExitCode "dot" ["-Tsvg", T.unpack $ "-K" <> layout] (T.unpack content)
                return $ case ec of
                           ExitSuccess -> RawBlock (Format "html")
                                 ("<div id=\"" <> identifier <> (T.pack $ "\" class=\"graphviz\">" <> out <> "</div>"))
                           _ -> CodeBlock (identifier, classes, keyvals) (T.pack err)
          Format "latex" ->
              do
                (_, out, err) <- readProcessWithExitCode
                                  "dot2tex" ["--figonly", T.unpack $ "--progoptions=-K" <> layout] (T.unpack content)
                return $ if length err == 0
                         then RawBlock (Format "latex") (T.pack out)
                         else CodeBlock (identifier, classes, keyvals) (T.pack err)
          _ ->
              do
                (ec, _, err) <- readProcessWithExitCode
                                  "dot" ["-Tpng", T.unpack $ "-K" <> layout, "-o" <> filename] (T.unpack content)
                return $ case ec of
                           ExitSuccess -> Para [Image (identifier, [], []) [Str content] (T.pack filename, identifier)]
                           _ -> CodeBlock (identifier, classes, keyvals) (T.pack err)
              where
                uniq = ((showDigest . sha256 . fromString) (T.unpack $ layout <> "/" <> content))
                filename = "graphviz-" <> uniq <> ".png"
    where
      layout =
          case lookup "layout" keyvals of
            Just x -> x
            _ -> "dot"
graphvizBlock _ x = return x
