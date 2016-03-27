module Parser.LanguageParser
( Token
, pattern
, tokenizeString
) where

import Control.Monad
import Text.Regex.PCRE

type Token = (String, String)

pattern :: String
pattern =
  "(\\s+)|" ++ -- whitespace
  "([;])|" ++ -- terminator
  
  "[\"](.+?)[\"]|" ++ -- string
  
  "//(.*)$|" ++ -- comment
  
  "(-?[a-zA-Z_0-9][a-zA-Z0-9_:.]*)|" ++ -- identifier
  "[#]([a-zA-Z]*)|" ++ -- preprocess
  
  "([{}<>\\]=|])|" ++ -- operator
  "([^\\s]+)" -- invalid

groupNames :: [String]
groupNames = [ ""
             , "whitespace"
             , "terminator"
             , "string"
             , "comment"
             , "identifier"
             , "preprocess"
             , "operator"
             , "invalid" ]

tokenizeString :: String -> [Token]
tokenizeString buffer = do
    let matches = buffer =~ pattern
    match <- matches
    (i, matchValue, groupName) <- zip3 [0..] match groupNames
    guard (not $ null matchValue)
    guard (i > 1)
    guard (groupName /= "comment")
    return (groupName, matchValue)
