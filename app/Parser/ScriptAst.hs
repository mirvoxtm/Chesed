{-# LANGUAGE OverloadedStrings #-}

module Parser.ScriptAst where

data VNCommand
  = VNSetBackground FilePath
  | VNShowCharacter String String String String String FilePath
  | VNSay String String
  | VNMusic FilePath
  | VNStopMusic
  | VNHide
  deriving (Show)
