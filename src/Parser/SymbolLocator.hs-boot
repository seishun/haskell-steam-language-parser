module Parser.SymbolLocator where

import {-# SOURCE #-} Parser.TokenAnalyzer

data Symbol

lookupSymbol :: Node -> String -> Bool -> Symbol
