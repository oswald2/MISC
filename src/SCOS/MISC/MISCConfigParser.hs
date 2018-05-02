{-# LANGUAGE OverloadedStrings #-}
module SCOS.MISC.MISCConfigParser
  ( MISCconfig
  , DynVar(..)
  , parseConfigFile
  , getResourceStatic
  , getResourceDynamic
  )
where


import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as T
import qualified Data.Text.Lazy.IO             as T

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM 


data DynVar = DynVar {
    dynName :: !Text,
    dynValue :: !Text
}

data MISCconfig = MISCconfig {
    miscStatic :: HashMap Text Text,
    miscDynamic :: HashMap Text DynVar
}


parseConfigFile :: FilePath -> IO MISCconfig
parseConfigFile path = do
    content <- T.readFile path
    let cont = T.lines content
    return (go False cont HM.empty HM.empty)
    where 
        go :: Bool -> [Text] -> HashMap Text Text -> HashMap Text DynVar -> MISCconfig
        go _ [] stat dyn = MISCconfig stat dyn
        go False (line:ls) stat dyn  
            | T.strip line == "" = go False ls stat dyn
            | line `T.index` 0 == '#' = go False ls stat dyn
            | T.strip line == "DYNAMIC" = go True ls stat dyn
            | otherwise = 
                let val = case T.words line of
                        [name, value] -> Just (name, value)
                        _ -> Nothing
                    newStat = case val of 
                        Nothing -> stat
                        Just (name, value) -> HM.insert name value stat
                in  
                go True ls newStat dyn
        go True (line:ls) stat dyn 
            | T.strip line == "" = go True ls stat dyn
            | line `T.index` 0 == '#' = go True ls stat dyn
            | otherwise = 
                let val = 
                        case T.words line of
                            [name, _packet, _permanent, value] -> Just (name, value)
                            _ -> Nothing
                    newDyn = case val of
                        Nothing -> dyn 
                        Just (name, value) -> HM.insert name (DynVar name value) dyn
                in 
                go True ls stat newDyn



getResourceStatic :: MISCconfig -> Text -> Maybe Text
getResourceStatic conf name = HM.lookup name (miscStatic conf)

getResourceDynamic :: MISCconfig -> Text -> Maybe DynVar
getResourceDynamic conf name = HM.lookup name (miscDynamic conf)
