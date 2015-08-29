data DataInt = D Int
               deriving (Eq, Ord, Show)
newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)


-- | testing

-- λ> case D undefined of D _ -> 1
-- 1
-- λ> case N undefined of N _ -> 1
-- 1
-- λ> case undefined of N _ -> 1
-- 1
-- λ> case undefined of D _ -> 1
-- *** Exception: Prelude.undefined

-- | 1. newtype acts like type casing in C?
-- |    int a = (float) b
-- | 2. newtype do the binding only in compile stage,
-- |    nothing in runtime, ie, no constructur in runtime.

