-- http://sneezy.cs.nott.ac.uk/fplunch/weblog/?p=69
-- TODO:

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x) = f x
either _ g (Right y) = g y

class MyFunctor f => Decisive f where
  nogood :: f Void -> Void
  orwell :: f (Either s t) -> Either (f s) (f t)

instance MyComonad c => MyDecisive c where
  nogood = counit
  orwell story = case counit story of Left s -> funMap (either id (const s)) story
                                      Right t -> funMap (either (const t) id) story
