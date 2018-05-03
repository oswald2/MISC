{-# LANGUAGE OverloadedStrings #-}
module SCOS.MISC.MISCConfigParser
  ( MISCconfig
  , DynamicVar(..)
  , parseConfigFile
  , getResourceStatic
  , getResourceDynamic
  )
where


import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T
import           Text.Parsec

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM


data DynamicVar = DynamicVar {
    dynVar :: T.Text,
    dynPersistent :: Char,
    dynMirror :: Char,
    dynParam :: Maybe T.Text,
    dynValue :: T.Text
    } deriving Show


data MISCconfig = MISCconfig {
    miscStatic :: HashMap Text Text,
    miscDynamic :: HashMap Text DynamicVar
}


parseConfigFile :: FilePath -> IO MISCconfig
parseConfigFile path = do
    content <- T.readFile path
    let cont = T.lines content
    return (go False cont HM.empty HM.empty)
  where
    go
        :: Bool
        -> [Text]
        -> HashMap Text Text
        -> HashMap Text DynVar
        -> MISCconfig
    go _ [] stat dyn = MISCconfig stat dyn
    go False (line : ls) stat dyn
        | T.strip line == ""
        = go False ls stat dyn
        | line `T.index` 0 == '#'
        = go False ls stat dyn
        | T.strip line == "DYNAMIC"
        = go True ls stat dyn
        | otherwise
        = let [name, value] = T.words line
              newStat       = HM.insert name value stat
          in  go True ls newStat dyn
    go True (line : ls) stat dyn
        | T.strip line == ""
        = go True ls stat dyn
        | line `T.index` 0 == '#'
        = go True ls stat dyn
        | otherwise
        = let [name, _permanent, _packet, value] = T.words line
              newDyn = HM.insert name (DynVar name value) dyn
          in  go True ls stat newDyn



getResourceStatic :: MISCconfig -> Text -> Maybe Text
getResourceStatic conf name = HM.lookup name (miscStatic conf)

getResourceDynamic :: MISCconfig -> Text -> Maybe DynVar
getResourceDynamic conf name = HM.lookup name (miscDynamic conf)




staticVariable = do
    i <- identifier
    v <- try (endOfLine >> return T.empty) <|> do
        spaces
        v <- value
        endOfLine
        return v
    return (Just (StaticVar i v))

dynamicVariable = do
    i <- identifier
    spaces
    p <- oneOf "NY"
    spaces
    m          <- oneOf "NY"

    (v, param) <- try (endOfLine >> return (T.empty, Nothing)) <|> parseValue m
    return (Just (DynamicVar i p m param v))
  where
    parseValue 'Y' = withParam
    parseValue _   = valueOnly
    withParam = do
        spaces
        d <- identifier
        spaces
        v <- value
        endOfLine
        return (v, Just d)
    valueOnly = do
        spaces
        v <- value
        endOfLine
        return (v, Nothing)

identifier :: Parser T.Text
identifier = T.pack <$> many1 (oneOf baseChars)


value :: Parser T.Text
value = T.strip . T.pack <$> many (oneOf valBaseChars)




baseChars =
    ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ "_-$%^&*()~`':;\"|/?<>[]{}.,"
valBaseChars =
    ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ " _-=+$%^&*()~`':;\"|/?<>[]{}.,\t"
