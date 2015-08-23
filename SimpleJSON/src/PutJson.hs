module PutJSON where

import SimpleJSON
import Data.List (intercalate)

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber n) = show n
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull) = "null"
renderJValue (JArray xs)
    = "[" ++ values ++ "]"
      where
        values = intercalate ", " (renderJValue <$> xs)
renderJValue (JObject o)
    = "{" ++ pairs ++ "}"
      where
        pairs = intercalate ", " (renderPairs <$> o)
        renderPairs (s, v) = "(" ++ s ++ ", " ++ renderJValue v ++ ")"

printJValue :: JValue -> IO ()
printJValue = putStrLn . renderJValue


