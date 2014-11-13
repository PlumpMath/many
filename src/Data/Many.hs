module Data.Many
    ( Many2 (NoneOf2, OneOf2a, OneOf2b, AllOf2)
    , Many3 (NoneOf3, OneOf3a, OneOf3b, OneOf3c, AllOf3)
    ) where


--------------------------------------------------------------------------------
-- | Data.Many, fixed sized collections with advanced functionality
--   Author: ssadler@mashi.org
--------------------------------------------------------------------------------


import Control.Applicative
import Data.Monoid


data Many1 a = NoneOf1 | AllOf1 a
    deriving (Eq, Show, Ord)


data Many2 a b = NoneOf2 | OneOf2a a | OneOf2b b | ManyOf2 (Maybe a) (Maybe b) | AllOf2 a b
    deriving (Eq, Show, Ord)


data Many3 a b c = NoneOf3 | OneOf3a a | OneOf3b b | OneOf3c c | ManyOf3 (Maybe a) (Maybe b) (Maybe c) | AllOf3 a b c
    deriving (Eq, Show, Ord)


instance Monoid (Many1 a) where
    mempty = NoneOf1
    mappend x y = case (x, y) of
        (x', NoneOf1) -> x'
        (NoneOf1, y') -> y'
        (x',       _) -> x'


instance Monoid (Many2 a b) where
    mempty = NoneOf2
    mappend x y = cast2 $ ManyOf2 (xa <|> ya) (xb <|> yb)
      where
        (ManyOf2 xa xb) = toMany2 x
        (ManyOf2 ya yb) = toMany2 y


instance Monoid (Many3 a b c) where
    mempty = NoneOf3
    mappend x y = cast3 $ ManyOf3 (xa <|> ya) (xb <|> yb) (xc <|> yc)
      where
        (ManyOf3 xa xb xc) = toMany3 x
        (ManyOf3 ya yb yc) = toMany3 y


toMany2 :: Many2 a b -> Many2 a b
toMany2 m = case m of
    NoneOf2    -> ManyOf2 Nothing  Nothing
    OneOf2a a  -> ManyOf2 (Just a) Nothing
    OneOf2b b  -> ManyOf2 Nothing  (Just b)
    AllOf2 a b -> ManyOf2 (Just a) (Just b)
    many_      -> many_


toMany3 :: Many3 a b c -> Many3 a b c
toMany3 m = case m of
    NoneOf3      -> ManyOf3 Nothing  Nothing Nothing
    OneOf3a a    -> ManyOf3 (Just a) Nothing Nothing
    OneOf3b b    -> ManyOf3 Nothing  (Just b) Nothing
    OneOf3c c    -> ManyOf3 Nothing  Nothing  (Just c)
    AllOf3 a b c -> ManyOf3 (Just a) (Just b) (Just c)
    many_        -> many_


cast2 :: Many2 a b -> Many2 a b
cast2 m = case m of
    ManyOf2 Nothing  Nothing  -> NoneOf2
    ManyOf2 (Just a) (Just b) -> AllOf2 a b
    many_                     -> many_


cast3 :: Many3 a b c -> Many3 a b c
cast3 m = case m of
    ManyOf3 Nothing Nothing Nothing    -> NoneOf3
    ManyOf3 (Just a) (Just b) (Just c) -> AllOf3 a b c
    many_                              -> many_
