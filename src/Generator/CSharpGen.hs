module Generator.CSharpGen
( cSharpGen
) where

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.Char
import Data.List
import Data.Map hiding (map, null, filter)
import Data.Maybe
import Text.Read

import CodeGenerator
import Parser.SymbolLocator
import Parser.TokenAnalyzer

readerTypeMap :: Map String String
readerTypeMap = fromList
  [ ("byte", "Byte")
  , ("short", "Int16")
  , ("ushort", "UInt16")
  , ("int", "Int32")
  , ("uint", "UInt32")
  , ("long", "Int64")
  , ("ulong", "UInt64")
  , ("char", "Char")
  ]

cSharpGen :: CodeGen
cSharpGen = CodeGen
  { emitNamespace = \end nspace -> do
      if end then tell
        [ "}"
        , "#pragma warning restore 1591"
        , "#pragma warning restore 0219"
        ]
      else tell 
        [ "#pragma warning disable 1591" -- this will hide "Missing XML comment for publicly visible type or member 'Type_or_Member'"
        , "#pragma warning disable 0219" -- Warning CS0219: The variable `(variable)' is assigned but its value is never used
        , "using System;"
        , "using System.IO;"
        , "using System.Runtime.InteropServices;"
        , ""
        , "namespace " ++ nspace
        , "{"
        ]
  , emitSerialBase = \level supportsGC -> do
      let padding = replicate level '\t'
      
      tell $ map (padding ++)
        [ "public interface ISteamSerializable"
        , "{"
        , "\tvoid Serialize(Stream stream);"
        , "\tvoid Deserialize( Stream stream );"
        , "}"
        
        , "public interface ISteamSerializableHeader : ISteamSerializable"
        , "{"
        , "\tvoid SetEMsg( EMsg msg );"
        , "}"
        
        , "public interface ISteamSerializableMessage : ISteamSerializable"
        , "{"
        , "\tEMsg GetEMsg();"
        , "}"
        ]
      
      when supportsGC . tell $ map (padding ++)
        [ "public interface IGCSerializableHeader : ISteamSerializable"
        , "{"
        , "\tvoid SetEMsg( uint msg );"
        , "}"
        
        , "public interface IGCSerializableMessage : ISteamSerializable"
        , "{"
        , "\tuint GetEMsg();"
        , "}"
        ]
      
      tell [""]
  , emitNode = \n level -> do
      case n of
        ClassNode {} -> emitClassNode n level
        EnumNode {} -> emitEnumNode n level
        _ -> return ()
  , supportsNamespace = True
  , supportsUnsignedTypes = True
  }

emitType :: Maybe Symbol -> String
emitType sym = do
  case sym of
    Just (WeakSymbol identifier) -> identifier
    Just (StrongSymbol kurasu prop) -> case prop of
      Nothing -> name kurasu
      Just prop' -> name kurasu ++ "." ++ name prop'
    _ -> "INVALID"

emitMultipleTypes :: [Symbol] -> String
emitMultipleTypes syms = emitMultipleTypes' syms "|"

emitMultipleTypes' :: [Symbol] -> String -> String
emitMultipleTypes' syms operation =
  let identList = [identifier | WeakSymbol identifier <- syms]
  in intercalate (" " ++ operation ++ " ") identList

getUpperName :: String -> String
getUpperName name = toUpper (head name) : drop 1 name

emitEnumNode :: Node -> Int -> Writer [String] ()
emitEnumNode enode level = do
  let padding = replicate level '\t'
  
  when (flags enode == Just "flags") $ tell
    [ padding ++ "[Flags]" ]
  
  case taipu enode of
    taipu@(Just _) -> tell
      [ padding ++ "public enum " ++ name enode ++ " : " ++ emitType taipu ]
    Nothing -> tell
      [ padding ++ "public enum " ++ name enode ]
  
  tell [ padding ++ "{" ]
  
  forM_ (reverse $ childNodes enode) $ \prop -> do
    let lastValue = emitMultipleTypes (deforuto prop)
    
    case obsolete prop of
      Just obsolete ->
        if length obsolete > 0 then do
          tell [ padding ++ "\t[Obsolete( \"" ++ obsolete ++ "\" )]" ]
        else do
          tell [ padding ++ "\t[Obsolete]" ]
      Nothing -> return ()
    tell [ padding ++ "\t" ++ name prop ++ " = " ++ lastValue ++ "," ]
  
  tell [ padding ++ "}" ]

emitClassNode :: Node -> Int -> Writer [String] ()
emitClassNode cnode level = do
  emitClassDef cnode level False
  
  emitClassIdentity cnode (level + 1)
  
  baseSize <- emitClassProperties cnode (level + 1)
  emitClassConstructor cnode (level + 1)
  
  emitClassSerializer cnode (level + 1) baseSize
  emitClassDeserializer cnode (level + 1) baseSize
  
  emitClassDef cnode level True

emitClassDef :: Node -> Int -> Bool -> Writer [String] ()
emitClassDef cnode level end = do
  let padding = replicate level '\t'
  
  if end
  then do
    tell
      [ padding ++ "}"
      , ""
      ]
  else do
    let parent = case ident cnode of
          Just _ ->
            if "MsgGC" `isInfixOf` name cnode
            then "IGCSerializableMessage"
            else "ISteamSerializableMessage"
          Nothing ->
            if "Hdr" `isInfixOf` name cnode
            then
              if "MsgGC" `isInfixOf` name cnode
              then "IGCSerializableHeader"
              else "ISteamSerializableHeader"
            else "ISteamSerializable"
    
    when ("Hdr" `isInfixOf` name cnode) $ tell
      [ padding ++ "[StructLayout( LayoutKind.Sequential )]" ]
    
    tell $ map (padding++)
      [ "public class " ++ name cnode ++ " : " ++ parent
      , "{"
      ]

emitClassIdentity :: Node -> Int -> Writer [String] ()
emitClassIdentity cnode level = do
  let padding = replicate level '\t'
  
  case ident cnode of
    ident@(Just _) -> do
      let supressObsoletionWarning = isJust $ do
            let Just (StrongSymbol kurasu prop) = ident
            let Just propNode@(PropNode {}) = prop
            obsolete propNode
      
      when supressObsoletionWarning $ tell
        [ padding ++ "#pragma warning disable 0612" ]
      
      if "MsgGC" `isInfixOf` name cnode
      then tell
        [ padding ++ "public uint GetEMsg() { return " ++ emitType ident ++ "; }" ]
      else tell
        [ padding ++ "public EMsg GetEMsg() { return " ++ emitType ident ++ "; }" ]
      
      when supressObsoletionWarning $ tell
        [ padding ++ "#pragma warning restore 0612" ]
      
      tell [ "" ]
    Nothing -> when ("Hdr" `isInfixOf` name cnode) $ do
      if "MsgGC" `isInfixOf` name cnode
      then
        if not . null . filter (\node -> name node == "msg") $ childNodes cnode
        then tell
          [ padding ++ "public void SetEMsg( uint msg ) { this.Msg = msg; }"
          , ""
          ]
        else tell
          -- this is required for a gc header which doesn't have an emsg
          [ padding ++ "public void SetEMsg( uint msg ) { }"
          , ""
          ]
      else tell
        [ padding ++ "public void SetEMsg( EMsg msg ) { this.Msg = msg; }"
        , ""
        ]

emitClassProperties :: Node -> Int -> Writer [String] Int
emitClassProperties cnode level = do
  let padding = replicate level '\t'
  
  case parent cnode of
    parent@(Just _) -> tell
      [ padding ++ "public " ++ emitType parent ++ " Header { get; set; }" ]
    Nothing -> return ()
  
  baseClassSize <- liftM sum $ forM (reverse $ childNodes cnode) $ \prop -> do
    let typestr = emitType (taipu prop)
    let propName = getUpperName (name prop)
    
    if flags prop == Just "const"
    then do
      tell 
        [ padding ++ "public static readonly " ++ typestr ++ " " ++ propName ++ " = " ++ emitType (listToMaybe $ deforuto prop) ++ ";" ]
      return 0
    else do
      let size = getTypeSize prop
      
      tell [ padding ++ "// Static size: " ++ show size ]
      
      case (flags prop, typestr) of
        (Just "steamidmarshal", "ulong") -> tell $ map (padding++)
          [ "private " ++ typestr ++ " " ++ name prop ++ ";"
          , "public SteamID " ++ propName ++ " { get { return new SteamID( " ++ name prop ++ " ); } set { " ++ name prop ++ " = value.ConvertToUInt64(); } }"
          ]
        (Just "boolmarshal", "byte") -> tell $ map (padding++)
          [ "private " ++ typestr ++ " " ++ name prop ++ ";"
          , "public bool " ++ propName ++ " { get { return ( " ++ name prop ++ " == 1 ); } set { " ++ name prop ++ " = ( byte )( value ? 1 : 0 ); } }"
          ]
        (Just "gameidmarshal", "ulong") -> tell $ map (padding++)
          [ "private " ++ typestr ++ " " ++ name prop ++ ";"
          , "public GameID " ++ propName ++ " { get { return new GameID( " ++ name prop ++ " ); } set { " ++ name prop ++ " = value.ToUInt64(); } }"
          ]
        _ ->
          let typestr' = fromMaybe typestr $ do
                flagsOpt'@(_:_) <- flagsOpt prop
                temp <- readMaybe flagsOpt' :: Maybe Int
                return (typestr ++ "[]")
          in tell
            [ padding ++ "public " ++ typestr' ++ " " ++ propName ++ " { get; set; }" ]
      
      return size
  
  tell [ "" ]
  
  return baseClassSize

emitClassConstructor :: Node -> Int -> Writer [String] ()
emitClassConstructor cnode level = do
  let padding = replicate level '\t'
  
  tell $ map (padding++)
    [ "public " ++ name cnode ++ "()"
    , "{"
    ]
  
  case parent cnode of
    parent@(Just _) ->
      do tell $ map (padding++)
          [ "\tHeader = new " ++ emitType parent ++ "();"
          , "\tHeader.Msg = GetEMsg();"
          ]
    _ -> return ()
  
  forM_ (reverse $ childNodes cnode) $ \prop -> do
    let defsym = listToMaybe (deforuto prop)
    let defflags = flags prop
    
    let ctor =
          if defflags == Just "proto"
          then "new " ++ emitType (taipu prop) ++ "()"
          else
            if isNothing defsym
            then case flagsOpt prop of
              Just (_:_) -> "new " ++ emitType (taipu prop) ++ "[" ++ show (getTypeSize prop) ++ "]"
              Nothing -> "0"
            else emitType defsym
    
    let symname =
          if defflags `elem` map Just ["steamidmarshal", "gameidmarshal", "boolmarshal"]
          then name prop
          else getUpperName (name prop)
    
    unless (defflags == Just "const") $ tell
      [ padding ++ "\t" ++ symname ++ " = " ++ ctor ++ ";" ]
  
  tell [ padding ++ "}" ]

emitClassSerializer :: Node -> Int -> Int -> Writer [String] ()
emitClassSerializer cnode level baseSize = void $ runMaybeT $ do
  let padding = replicate level '\t'
  
  tell
    [ ""
    , padding ++ "public void Serialize(Stream stream)"
    , padding ++ "{"
    ]
  
  -- first emit variable length members
  
  -- if (cnode.Parent != null)
  
  openedStreams <- liftM catMaybes $ forM (reverse $ childNodes cnode) $ \prop -> do
    let typestr = emitType (taipu prop)
    let size = getTypeSize prop
    
    if size == 0 then do
      if flags prop == Just "proto"
      then do
        when (baseSize == 0) $ do
          -- early exit
          tell $ map (padding++)
            [ "\tProtoBuf.Serializer.Serialize<" ++ typestr ++ ">(stream, " ++ getUpperName (name prop) ++ ");"
            , "}"
            ]
          mzero
        
        tell $ map (padding++)
          [ "\tMemoryStream ms" ++ getUpperName (name prop) ++ " = new MemoryStream();"
          , "\tProtoBuf.Serializer.Serialize<" ++ typestr ++ ">(ms" ++ getUpperName (name prop) ++ ", " ++ getUpperName (name prop) ++ ");"
          ]
        
        case flagsOpt prop of
          Just flagsOpt -> tell
            [ padding ++ "\t" ++ getUpperName flagsOpt ++ " = (int)ms" ++ getUpperName (name prop) ++ ".Length;" ]
          Nothing -> return ()
      else do
        tell
          [ padding ++ "\tMemoryStream ms" ++ getUpperName (name prop) ++ " = " ++ getUpperName (name prop) ++ ".serialize();" ]
      
      return $ Just $ "ms" ++ getUpperName (name prop)
    
    else return Nothing
  
  tell [ padding ++ "\tBinaryWriter bw = new BinaryWriter( stream );" ]
  tell [ "" ]
  
  -- if (cnode.Parent != null)
  
  -- next emit writers
  forM_ (reverse $ childNodes cnode) $ \prop -> do
    let typecast = case taipu prop of
          Just (StrongSymbol enode@(EnumNode {}) prop) -> case taipu enode of
              Just (WeakSymbol identifier) -> "(" ++ identifier ++ ")"
              _ -> "(int)"
          _ -> ""
    
    let propName =
          if flags prop `elem` map Just ["steamidmarshal", "gameidmarshal", "boolmarshal"]
          then name prop
          else getUpperName (name prop)
    
    case flags prop of
      Just "proto" -> tell
        [ padding ++ "\tbw.Write( ms" ++ propName ++ ".ToArray() );" ]
      Just "const" -> return ()
      _ -> do
        let propName' = case flags prop of
              Just "protomask" -> "MsgUtil.MakeMsg( " ++ propName ++ ", true )"
              Just "protomaskgc" -> "MsgUtil.MakeGCMsg( " ++ propName ++ ", true )"
              _ -> propName
        
        tell [ padding ++ "\tbw.Write( " ++ typecast ++ propName' ++ " );" ]
  
  tell [""]
  
  forM_ openedStreams $ \stream -> tell
    [ padding ++ "\t" ++ stream ++ ".Close();" ]
  
  tell [ padding ++ "}" ]

emitClassDeserializer :: Node -> Int -> Int -> Writer [String] ()
emitClassDeserializer cnode level baseSize = do
  let padding = replicate level '\t'
  
  tell
    [ ""
    , padding ++ "public void Deserialize( Stream stream )"
    , padding ++ "{"
    ]
  
  when (baseSize > 0) $ tell
    [ padding ++ "\tBinaryReader br = new BinaryReader( stream );"
    , ""
    ]
  
  when (isJust $ parent cnode) $ tell
    [ padding ++ "\tHeader.Deserialize( stream );" ]
  
  forM_ (reverse $ childNodes cnode) $ \prop -> do
    let typestr = emitType (taipu prop)
    let size = getTypeSize prop
    
    let defflags = flags prop
    let symname =
          if defflags `elem` map Just ["steamidmarshal", "gameidmarshal", "boolmarshal"]
          then name prop
          else getUpperName (name prop)
    
    if defflags == Just "const"
    then return ()
    else do
      if size == 0
      then do
        if flags prop == Just "proto"
        then case flagsOpt prop of
          Just flagsOpt -> tell $ map (padding++)
            [ "\tusing( MemoryStream ms" ++ getUpperName (name prop) ++ " = new MemoryStream( br.ReadBytes( " ++ getUpperName flagsOpt ++ " ) ) )"
            , "\t\t" ++ getUpperName (name prop) ++ " = ProtoBuf.Serializer.Deserialize<" ++ typestr ++ ">( ms" ++ getUpperName (name prop) ++ " );"
            ]
          Nothing -> tell $ map (padding++)
            [ "\t" ++ getUpperName (name prop) ++ " = ProtoBuf.Serializer.Deserialize<" ++ typestr ++ ">( stream );" ]
        else tell $ map (padding++)
          [ "\t" ++ getUpperName (name prop) ++ ".Deserialize( stream );" ]
      else do
        let (typecast, typestr') =
              if not (member typestr readerTypeMap)
              then ("(" ++ typestr ++ ")", getTypeOfSize size $ supportsUnsignedTypes cSharpGen)
              else ("", typestr)
        
        let call = case flagsOpt prop of
              Just flagsOpt@(_:_) -> "br.Read" ++ (readerTypeMap ! typestr') ++ "s( " ++ flagsOpt ++ " )"
              _ -> "br.Read" ++ (readerTypeMap ! typestr') ++ "()"
        
        let call' = case flags prop of
              Just "protomask" -> "MsgUtil.GetMsg( (uint)" ++ call ++ " )"
              Just "protomaskgc" -> "MsgUtil.GetGCMsg( (uint)" ++ call ++ " )"
              _ -> call
        
        tell [ padding ++ "\t" ++ symname ++ " = " ++ typecast ++ call' ++ ";" ]
    
  tell [ padding ++ "}" ]
