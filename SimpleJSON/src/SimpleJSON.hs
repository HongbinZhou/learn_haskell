-- ref: http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html
module SimpleJSON
    (
     JValue(..)                 -- The special notation (..) that follows the name JValue indicates that we are exporting both the type and all of its constructors
    , getString
) where

import Test.QuickCheck

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

prop_getString s =
    let Just x = getString (JString s) in
    s == x

getDouble :: JValue -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _  = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _  = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray v) = Just v
getArray _  = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _  = Nothing

isNull :: JValue -> Bool
isNull n = n == JNull
