{-# LANGUAGE OverloadedStrings #-}

module GTP2.ParserSpec (spec) where

import GTP2.Parser

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 as A8

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "play_move black D3" $ do
    A.parseOnly command "play_move black D3" == Right (Nothing, "play_move", ["black", "D3"])
  it "1 boardsize 7" $ do
    A.parseOnly command "1 boardsize 7" == Right (Just 1, "boardsize", ["7"])
  it "4 genmove white" $ do
    A.parseOnly command "4 genmove white" == Right (Just 4, "genmove", ["white"])
  it "2 clear_board" $ do
    A.parseOnly command "2 clear_board" == Right (Just 2, "clear_board", [])
  it "= result\\n\\n" $ do
    A.parseOnly response "= result\n\n" == Right (Nothing, "result")
  it "=0 result\\n\\n" $ do
    A.parseOnly response "=0 result\n\n" == Right (Just 0, "result")
