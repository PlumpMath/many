
Data.Many
=========

fixed sized heterogenous monoids


```
 λ import Data.Many
 λ import Data.Monoid
 λ mempty :: Many2 a b
NoneOf2
 λ :t OneOf2a ()
OneOf2a 1 :: Many2 () b
 λ :t OneOf2a () <> OneOf2b 'c'
OneOf2a () <> OneOf2b 'c' :: Many2 () Char
 λ OneOf2a () <> OneOf2b 'c'
AllOf2 () 'c'
 λ OneOf3a "hello" <> OneOf3b 1
ManyOf3 (Just "hello") (Just 1) Nothing
```
