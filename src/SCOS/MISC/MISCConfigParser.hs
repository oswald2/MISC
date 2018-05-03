{-# LANGUAGE OverloadedStrings #-}
module SCOS.MISC.MISCConfigParser
  ( MISCconfig
  , DynamicVar(..)
  , StaticVar(..)
  , parseConfigFile
  , getResourceStatic
  , getResourceDynamic
  )
where

import Control.Monad (void)

import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Text.Parsec
import           Text.Parsec.Text

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


data DynamicVar = DynamicVar {
    dynVar :: !Text,
    dynPersistent :: !Char,
    dynMirror :: !Char,
    dynParam :: Maybe Text,
    dynValue :: !Text
    } deriving Show


data StaticVar = StaticVar {
    statVar :: !Text,
    statValue :: !Text
} deriving Show

data MISCconfig = MISCconfig {
    miscStatic :: HashMap Text StaticVar,
    miscDynamic :: HashMap Text DynamicVar
}


parseConfigFile :: FilePath -> IO (Either Text MISCconfig)
parseConfigFile path = do
    content <- T.readFile path
    let cont = T.lines content
    return (go False cont HM.empty HM.empty)
  where
    go
        :: Bool
        -> [Text]
        -> HashMap Text StaticVar
        -> HashMap Text DynamicVar
        -> Either Text MISCconfig
    go _ [] stat dyn = Right (MISCconfig stat dyn)
    go False (line : ls) stat dyn
        | T.strip line == "" = go False ls stat dyn
        | line `T.index` 0 == '#' = go False ls stat dyn
        | T.strip line == "DYNAMIC" = go True ls stat dyn
        | otherwise = case parse staticVariable "" (T.toStrict line) of
            Left err -> Left (T.pack (show err))
            Right new ->
                let hm = HM.insert (statVar new) new stat in go False ls hm dyn
    go True (line : ls) stat dyn
        | T.strip line == "" = go True ls stat dyn
        | line `T.index` 0 == '#' = go True ls stat dyn
        | otherwise = case parse dynamicVariable "" (T.toStrict line) of
            Left err -> Left (T.pack (show err))
            Right new ->
                let hm = HM.insert (dynVar new) new dyn in go True ls stat hm


getResourceStatic :: MISCconfig -> Text -> Maybe StaticVar
getResourceStatic conf name = HM.lookup name (miscStatic conf)

getResourceDynamic :: MISCconfig -> Text -> Maybe DynamicVar
getResourceDynamic conf name = HM.lookup name (miscDynamic conf)



staticVariable :: Parser StaticVar
staticVariable = do
    i <- identifier
    v <- try (endOfLine >> return T.empty) <|> do
        spaces
        v <- value
        void $ endOfLine
        return v
    pure (StaticVar i v)

dynamicVariable :: Parser DynamicVar
dynamicVariable = do
    i <- identifier
    spaces
    p <- oneOf "NY"
    spaces
    m          <- oneOf "NY"
    (v, param) <- try (endOfLine >> return (T.empty, Nothing)) <|> parseValue m
    pure (DynamicVar i p m param v)
  where
    parseValue 'Y' = withParam
    parseValue _   = valueOnly
    withParam = do
        spaces
        d <- identifier
        spaces
        v <- value
        void $ endOfLine
        return (v, Just d)
    valueOnly = do
        spaces
        v <- value
        void $ endOfLine
        return (v, Nothing)

identifier :: Parser Text
identifier = T.pack <$> many1 (oneOf baseChars)


value :: Parser Text
value = T.strip . T.pack <$> many (oneOf valBaseChars)



baseChars :: [Char]
baseChars =
    ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ "_-$%^&*()~`':;\"|/?<>[]{}.,"

valBaseChars :: [Char]
valBaseChars =
    ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ " _-=+$%^&*()~`':;\"|/?<>[]{}.,\t"
