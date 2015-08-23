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
