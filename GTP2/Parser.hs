{-# LANGUAGE OverloadedStrings #-}

module GTP2.Parser (command, response) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 as A8

import Test.Hspec

------------------------------------------------------------------------

response :: A.Parser (Maybe Int, ByteString)
response = A.word8 61 *>
  ((,) <$> optional command_id
       <*> (sp *> text) <* (lf <* lf))

------------------------------------------------------------------------

command :: A.Parser (Maybe Int, ByteString, [ByteString])
command = (,,) <$> optional (command_id <* sp)
               <*> command_name
               <*> A.many' (sp *> command_arg)

command_id :: A.Parser Int
command_id = A8.decimal

command_name :: A.Parser ByteString
command_name = text

command_arg :: A.Parser ByteString
command_arg = text

------------------------------------------------------------------------

ht = A.word8  9 -- horizontal tab
cr = A.word8 13 -- carriage return
lf = A.word8 10 -- line feed
sp = A.word8 32 -- space

text :: A.Parser ByteString
text = A.takeWhile1 p where
  p c = (c >= 97 && c <= 122) ||
        (c >= 48 && c <=  57) ||
        (c >= 65 && c <=  90) ||
        (c == 95)
