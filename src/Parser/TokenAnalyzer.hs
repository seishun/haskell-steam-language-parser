module Parser.TokenAnalyzer
( Node(..)
, analyze
) where

import Control.Monad.Trans.State
import Parser.LanguageParser
import {-# SOURCE #-} Parser.SymbolLocator

data Node = Node
    { childNodes :: [Node] -- reverse order because reasons (in others too)
    } | ClassNode
    { childNodes :: [Node]
    , name       :: String
    , ident      :: Maybe Symbol
    , parent     :: Maybe Symbol
    } | PropNode
    { childNodes :: [Node]
    , name       :: String
    , flags      :: Maybe String
    , flagsOpt   :: Maybe String
    , taipu      :: Maybe Symbol
    , deforuto   :: [Symbol]
    , obsolete   :: Maybe String
    } | EnumNode
    { childNodes :: [Node]
    , name       :: String
    , flags      :: Maybe String
    , taipu      :: Maybe Symbol
    }

analyze :: [Token] -> IO Node
analyze = analyze' (Node [])

analyze' :: Node -> [Token] -> IO Node
analyze' root (("EOF", _):tokens) = analyze' root tokens
analyze' root (("preprocess", curValue):tokens) =
    let ((_, textValue), tokens') = expect "string" tokens
    in if curValue == "import"
    then do
        parentTokens <- tokenizeString <$> readFile textValue
        newRoot <- analyze' (Node []) parentTokens
        analyze' (Node $ childNodes newRoot ++ childNodes root) tokens'
    else analyze' root tokens'
analyze' root (("identifier", curValue):tokens) = case curValue of
    "class" ->
        let (root', tokens') = flip runState tokens $ do
                (_, nameValue) <- state (expect "identifier")
                
                op1 <- state (optional' "operator" "<")
                ident <- case op1 of
                    Just _ -> do
                        ident <- state (expect "identifier")
                        op2 <- state (expect' "operator" ">")
                        return (Just ident)
                    Nothing -> return Nothing
                
                expects <- state (optional' "identifier" "expects")
                parent <- case expects of
                    Just _ -> Just <$> state (expect "identifier")
                    Nothing -> return Nothing
                
                let ident' = do
                      (_, value) <- ident
                      return (lookupSymbol root value False)
                
                let parent' = Nothing -- blame VoiDeD
                
                let cnode = ClassNode [] nameValue ident' parent'
                parseInnerScope root { childNodes = cnode : childNodes root }
        in analyze' root' tokens'
    "enum" ->
        let (root', tokens') = flip runState tokens $ do
                (_, nameValue) <- state (expect "identifier")
                
                op1 <- state (optional' "operator" "<")
                datatype <- case op1 of
                    Just _ -> do
                        datatype <- state (expect "identifier")
                        op2 <- state (expect' "operator" ">")
                        return (Just datatype)
                    Nothing -> return Nothing
                
                flag <- state (optional' "identifier" "flags")
                
                let flags = do
                        (_, value) <- flag
                        return value
                
                let taipu = do
                        (_, value) <- datatype
                        return (lookupSymbol root value False)
                
                let enode = EnumNode [] nameValue flags taipu
                parseInnerScope root { childNodes = enode : childNodes root }
        in analyze' root' tokens'
    _ -> analyze' root tokens
analyze' root (_:tokens) = analyze' root tokens
analyze' root [] = return root

parseInnerScope :: Node -> State [Token] Node
parseInnerScope root = do
  scope1 <- state (expect' "operator" "{")
  state (parseInnerScope' root)

parseInnerScope' :: Node -> [Token] -> (Node, [Token])
parseInnerScope' root tokens = case optional' "operator" "}" tokens of
    (Just scope2, tokens') -> (root, tokens')
    (Nothing, ((_, t1Value):tokens')) -> flip runState tokens' $ do
        t1op <- state (optional' "operator" "<")
        
        flagsOpt <- case t1op of
            Just _ -> do
                (_, flagopValue) <- state (expect "identifier")
                state (expect' "operator" ">")
                return (Just flagopValue)
            Nothing -> return Nothing
        
        t2 <- state (optional "identifier")
        t3 <- state (optional "identifier")
        
        let (name, taipu, flags) = case t2 of
                Nothing -> (t1Value, Nothing, Nothing)
                Just (_, t2Value) -> case t3 of
                    Nothing -> (t2Value, Just (lookupSymbol root t1Value False), Nothing)
                    Just (_, t3Value) -> (t3Value, Just (lookupSymbol root t2Value False), Just t1Value)
        
        defop <- state (optional' "operator" "=")
        
        deforuto <- case defop of
            Just _ ->
                let getDefaults ((_, valueValue):tokens) =
                        let deforuto = lookupSymbol root valueValue False
                        in case optional' "operator" "|" tokens of
                            (Just _, tokens') ->
                                let (defaults, tokens'') = getDefaults tokens'
                                in (deforuto:defaults, tokens'')
                            (Nothing, _) ->
                                let (_, tokens') = expect' "terminator" ";" tokens
                                in ([deforuto], tokens')
                in state getDefaults
            Nothing -> state (expect' "terminator" ";") >> return []
        
        obsolete <- state (optional' "identifier" "obsolete")
        obsolete' <- case obsolete of
            Just _ -> do
                obsoleteReason <- state (optional "string")
                case obsoleteReason of
                    Just (_, value) -> return (Just value)
                    Nothing -> return (Just "")
            Nothing -> return Nothing
        
        let pnode = PropNode [] name flags flagsOpt taipu deforuto obsolete'
        -- insert it in the beginning of the children of the first child of root
        -- that's the enode/cnode we're parsing inner scope of
        let (parent:xs) = childNodes root
        let root' = root {childNodes = parent { childNodes = pnode : childNodes parent } : xs}
        state (parseInnerScope' root')

expect :: String -> [Token] -> (Token, [Token])
expect name (peek:tokens)
    | fst peek == name = (peek, tokens)
    | otherwise = error ("Expecting " ++ name)

expect' :: String -> String -> [Token] -> (Token, [Token])
expect' name value (peek:tokens)
    | peek == (name, value) = (peek, tokens)
    | otherwise = error ("Expecting " ++ name ++ " '" ++ value ++ "', but got '" ++ snd peek ++ "'")

optional :: String -> [Token] -> (Maybe Token, [Token])
optional name tokens@(peek:tokens')
    | fst peek == name = (Just peek, tokens')
    | otherwise = (Nothing, tokens)

optional' :: String -> String -> [Token] -> (Maybe Token, [Token])
optional' name value tokens@(peek:tokens')
    | peek == (name, value) = (Just peek, tokens')
    | otherwise = (Nothing, tokens)
