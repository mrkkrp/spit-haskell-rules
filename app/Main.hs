{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import Data.Char (toUpper, chr)
import Data.List (unfoldr)
import Options.Applicative
import Text.Mustache
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
spitPackage isTop n i = do
  let name = getName i
      ruleFile =
        if isTop
          then "BUILD"
          else name ++ ".BUILD"
      moduleName = formModuleName name
      moduleFile = moduleName ++ ".hs"
  TL.writeFile ruleFile . renderMustache buildTemplate . object $
    [ "insert_toolchain" .= isTop
    , "library_name"     .= name
    , "srcs"             .= [moduleFile]
    , "deps"             .= ([] :: [String]) -- TODO
    ]
  TL.writeFile moduleFile . renderMustache moduleTemplate . object $
    [ "module_name"      .= moduleName
    ]

getName :: Int -> String
getName n = unfoldr f (Just n)
  where
    f Nothing = Nothing
    f (Just i) = Just $
      let (q,r) = i `quotRem` 26
      in if q > 0
           then ('z', Just (i - 26))
           else (chr (r + 97), Nothing)

getNamesFromTo :: Int -> Int -> [String]
getNamesFromTo n m = getName <$> [n..m]

formModuleName :: String -> String
formModuleName ""     = ""
formModuleName (x:xs) = toUpper x : xs

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
