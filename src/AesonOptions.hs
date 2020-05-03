module AesonOptions where

import Data.Aeson.TH
import Data.Char
import RIO

options :: Options
options = defaultOptions
    { fieldLabelModifier = dropUntilFirstCapital
    , constructorTagModifier = constructorTagModifier defaultOptions
    , allNullaryToStringTag = allNullaryToStringTag defaultOptions
    , omitNothingFields = omitNothingFields defaultOptions
    , sumEncoding = sumEncoding defaultOptions
    , unwrapUnaryRecords = unwrapUnaryRecords defaultOptions
    , tagSingleConstructors = tagSingleConstructors defaultOptions
    }

dropUntilFirstCapital :: String -> String
dropUntilFirstCapital [] = []
dropUntilFirstCapital ('_' : tl) = dropUntilFirstCapital tl
dropUntilFirstCapital (h:tl)
    | isUpper h = toLower h : tl
    | otherwise = dropUntilFirstCapital tl
