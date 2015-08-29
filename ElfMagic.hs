import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C

hasElfMagic :: L.ByteString -> Bool
hasElfMagic = L.isPrefixOf $ L.pack [0x7f, 0x45, 0x4c, 0x46]

test_hasElfMagic :: String -> Bool
test_hasElfMagic = hasElfMagic . C.pack
