module CodeGenerator
( CodeGen(..)
, getTypeOfSize
, getTypeSize
, emitCode
) where

import Control.Monad.Writer
import Data.Map hiding (size)
import Parser.SymbolLocator
import Parser.TokenAnalyzer

data CodeGen = CodeGen
  { emitNamespace :: Bool -> String -> Writer [String] ()
  , emitSerialBase :: Int -> Bool -> Writer [String] ()
  , emitNode :: Node -> Int -> Writer [String] ()
  
  , supportsNamespace :: Bool
  , supportsUnsignedTypes :: Bool
  }

data TypeInfo = Signed { size :: Int }
  | Unsigned { size :: Int, signedType :: String }

defaultType :: String
defaultType = "uint"

weakTypeMap :: Map String TypeInfo
weakTypeMap = fromList
  [ ("byte", Signed 1)
  , ("short", Signed 2)
  , ("ushort", Unsigned 2 "short")
  , ("int", Signed 4)
  , ("uint", Unsigned 4 "int")
  , ("long", Signed 8)
  , ("ulong", Unsigned 8 "long")
  ]

getTypeOfSize :: Int -> Bool -> String
getTypeOfSize size' unsigned = f "bad" weakTypeMap
  where f = foldrWithKey $ \key value continue ->
          if size value == size'
          then case value of
            Signed {} -> key
            Unsigned {} ->
              if unsigned
              then key
              else signedType value
          else continue

getTypeSize :: Node -> Int
getTypeSize prop =
  let sym = taipu prop
  
  -- no static size for proto
  in if flags prop == Just "proto"
  then 0
  else case sym of
    Just (WeakSymbol identifier) ->
      let key =
            if member identifier weakTypeMap
            then identifier
            else defaultType
      in case flagsOpt prop of
        Just flagsOpt@(_:_) -> read flagsOpt
        _ -> size (weakTypeMap ! key)
    
    Just (StrongSymbol kurasu prop) ->
      case kurasu of
        enode@(EnumNode {}) ->
          case taipu enode of
            Just taipu@(WeakSymbol {}) -> size ( weakTypeMap ! identifier taipu)
            _ -> size (weakTypeMap ! defaultType)
        _ -> 0
    
    _ -> 0

emitCode :: Node -> CodeGen -> String -> Bool -> Bool -> Writer [String] ()
emitCode root gen nspace supportsGC internalFile = do
    emitNamespace gen False nspace
    
    let level = if supportsNamespace gen then 1 else 0
    
    when internalFile (emitSerialBase gen level supportsGC)
    
    forM_ (reverse $ childNodes root) $ \n -> emitNode gen n level
    
    emitNamespace gen True nspace
