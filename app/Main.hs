{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Control.Monad
import Crypto.Hash.Algorithms (SHA256)
import Data.Aeson
import Data.ByteString (ByteString)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Mustache
import qualified Crypto.Hash              as H
import qualified Data.ByteString.Char8    as B
import qualified Data.Text.Lazy.IO        as TL
import qualified Text.Mustache.Compile.TH as TH

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  let total = optsWidth * optsDepth
  forM_ [0..total - 1] (spitPackage False optsDepth)
  spitPackage True optsDepth total
  TL.writeFile "WORKSPACE" (renderMustache workspaceTemplate Null)

----------------------------------------------------------------------------
-- The algorithm

spitPackage
  :: Bool              -- ^ Whether it's the top level target
  -> Int               -- ^ Depth
  -> Int               -- ^ Current index
  -> IO ()
spitPackage isTop depth i = do
  let name = getName i
      ruleFile =
        if isTop
          then "BUILD"
          else name </> "BUILD"
      moduleName = formModuleName name
      moduleFileLocal = moduleName ++ ".hs"
      moduleFile =
        if isTop
          then moduleFileLocal
          else name </> moduleFileLocal
      i' = i - i `rem` depth
  unless isTop $
    createDirectoryIfMissing True name
  TL.writeFile ruleFile . renderMustache buildTemplate . object $
    [ "insert_toolchain" .= isTop
    , "library_name"     .= name
    , "srcs"             .= [moduleFileLocal]
    , "deps"             .=
      if isTop
        then getName <$> takeWhile (< i) (iterate (+ depth) (depth - 1))
        else if i' == i
               then []
               else [getName (i - 1)]
    ]
  TL.writeFile moduleFile . renderMustache moduleTemplate . object $
    [ "module_name"      .= moduleName
    ]

getName :: Int -> String
getName = show . H.hash @ByteString @SHA256 . B.pack . show

formModuleName :: String -> String
formModuleName = ("Module_" ++)

----------------------------------------------------------------------------
-- Templates

workspaceTemplate :: Template
workspaceTemplate = $(TH.compileMustacheFile "data/WORKSPACE.mustache")

buildTemplate :: Template
buildTemplate = $(TH.compileMustacheFile "data/BUILD.mustache")

moduleTemplate :: Template
moduleTemplate = $(TH.compileMustacheFile "data/module.mustache")

----------------------------------------------------------------------------
-- Command line options

data Opts = Opts
  { optsWidth :: Int
  , optsDepth :: Int
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (helper <*> optsParser) . mconcat $
  [ fullDesc
  ]

optsParser :: Parser Opts
optsParser = Opts
  <$> (option auto . mconcat)
    [ long    "width"
    , short   'w'
    , metavar "W"
    , help    "On how many libraries the top-level target should depend"
    ]
  <*> (option auto . mconcat)
    [ long    "depth"
    , short   'd'
    , metavar "D"
    , help    "Length of the chain of linear dependencies"
    ]
