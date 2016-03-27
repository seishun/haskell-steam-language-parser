module Parser.SymbolLocator
( Symbol(..)
, identifierPattern
, fullIdentPattern
, lookupSymbol
) where

import Data.List
import Data.Maybe
import Parser.TokenAnalyzer
import Text.Regex.PCRE

data Symbol = StrongSymbol { kurasu :: Node, prop :: Maybe Node }
            | WeakSymbol { identifier :: String }

identifierPattern :: String
identifierPattern = "([a-zA-Z0-9_:]*)"

fullIdentPattern :: String
fullIdentPattern = "([a-zA-Z0-9_]*?)::([a-zA-Z0-9_]*)"

findNode :: Node -> String -> Maybe Node
findNode tree symbol = listToMaybe
                     $ filter (\child -> name child == symbol)
                     $ childNodes tree

lookupSymbol :: Node -> String -> Bool -> Symbol
lookupSymbol tree identifier strongonly =
    let ident = case identifier =~ identifierPattern of
            [] -> error ("Invalid identifier specified " ++ identifier)
            (match:_) -> match
    in if not ("::" `isInfixOf` identifier)
        then case findNode tree (ident !! 0) of
            Nothing -> if strongonly
                then error ("Invalid weak symbol " ++ identifier)
                else WeakSymbol identifier
            Just classNode -> StrongSymbol classNode Nothing
        else
            let [_, kurasu, name] = case identifier =~ fullIdentPattern of
                    [] -> error "Couldn't parse full identifier"
                    (match:_) -> match
            in case findNode tree kurasu of
                Nothing -> error ("Invalid class in identifier " ++ identifier)
                Just classNode -> case findNode classNode name of
                    Nothing -> error ("Invalid property in identifier " ++ identifier)
                    Just propNode -> StrongSymbol classNode (Just propNode)

