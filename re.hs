
-- | preparation:
-- |   stack install regex-posix
-- |   stack install regex-tdfa

import qualified Text.Regex.TDFA as RT
import qualified Text.Regex.Posix as RP


-- | test_RT will return true
test_RT :: Bool
test_RT = "周一二.mp3" RT.=~ "^周.*\\.mp3$" :: Bool

-- | rest_RP will return false. NOT expected!
test_RP :: Bool
test_RP = "周一二.mp3" RP.=~ "^周.*\\.mp3$" :: Bool
