-- http://tonymorris.github.io/blog//posts/automated-validation-with-applicatives-and-semigroups-for-sanjiv/index.html

data Validation e a = Fail e
                    | Success a deriving (Eq, Show)

-- Associative, binary operation (Monoid without identity)
class Semigroup a where
  append :: a -> a -> a

(<+>) :: Validation e a -> (a -> b) -> Validation e b
Fail e <+> _ = Fail e
Success a <+> f = Success (f a)

(<*>) :: (Semigroup e) => Validation e a -> Validation e (a -> b) -> Validation e b
Fail e1 <*> Fail e2 = Fail (e1 `append` e2)
Fail e1 <*> Success _ = Fail e1
Success _ <*> Fail e2 = Fail e2
Success a <*> Success f = Success (f a)
