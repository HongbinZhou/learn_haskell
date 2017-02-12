{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Exts (IsList(..))

data Node a = Branch3 a a a
  | Branch2 a a
  deriving Show

instance IsList (Node a) where
  type Item (Node a) = a

  toList (Branch2 x y) = [x, y]
  toList (Branch3 x y z) = [x, y, z]

  fromList [x, y] = Branch2 x y
  fromList [x, y, z] = Branch3 x y z
  fromList _ = error "Node must contain two or three elements"

data Affix a = One a
  | Two a a
  | Three a a a
  | Four a a a a
  deriving Show

instance IsList (Affix a) where
  type Item (Affix a) = a

  toList (One x) = [x]
  toList (Two x y) = [x, y]
  toList (Three x y z) = [x, y, z]
  toList (Four x y z w) = [x, y, z, w]

  fromList [x] = One x
  fromList [x, y] = Two x y
  fromList [x, y, z] = Three x y z
  fromList [x, y, z, w] = Four x y z w
  fromList _ = error "Affix must have 1 - 4 elements"

affixPrepend :: a -> Affix a -> Affix a
affixPrepend x ax= fromList (x:(toList ax))

affixAppend :: a -> Affix a -> Affix a
affixAppend x ax = fromList ((toList ax) ++ [x])

data FingerTree a
  = Empty
  | Single a
  | Deep {
      prefix :: Affix a,
      deeper :: FingerTree (Node a),
      suffix :: Affix a
      }
  deriving Show

infixr 5 <|
(<|) :: a -> FingerTree a -> FingerTree a
x <| Empty = Single x
x <| Single y = Deep (One x) Empty (One y)
x <| Deep (Four a b c d) deeper suffix = Deep (Two x a) (node <| deeper) suffix
  where node = Branch3 b c d
x <| tree = tree {prefix = affixPrepend x (prefix tree)}

infixl 4 |>
(|>) :: FingerTree a -> a -> FingerTree a
Empty |> x = Single x
Single x |> y = Deep (One x) Empty (One y)
Deep prefix deeper (Four a b c d) |> y = Deep prefix (deeper |> node) (Two d y)
  where node = Branch3 a b c
tree |> y = tree {suffix = affixAppend y (suffix tree)}

empty :: FingerTree a
empty = Empty

mytree :: FingerTree Int
mytree = 1 <| empty |> 2 |> 3 |> 4 |> 5 |> 6 |> 7


layer1 :: FingerTree Char
layer1 = Deep prefix deeper suffix
  where prefix = fromList ['t', 'h']
        deeper = layer2
        suffix = fromList ['r', 'e', 'e']

layer2 :: FingerTree (Node Char)
layer2 = Deep prefix deeper suffix
  where prefix = fromList [Branch2 'i' 's',
                           Branch2 'i' 's']
        deeper = layer3
        suffix = fromList [Branch3 'n' 'o' 't',
                           Branch2 'a' 't']

layer3 :: FingerTree a
layer3 = Empty

b3 = Branch3 1 2 3
b2 = Branch2 4 5

b = Branch2 b3 b2

main = do
  print "hi"
