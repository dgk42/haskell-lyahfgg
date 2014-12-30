-- http://stackoverflow.com/questions/23895891/is-this-a-functor-or-monad

import MyFunctor

data Promise a b = PendingPromise (a -> b)
                 | ResolvedPromise b
                 | BrokenPromise

instance MyFunctor (Promise c) where
    funMap f (PendingPromise g)  = PendingPromise (f . g)
    funMap f (ResolvedPromise a) = ResolvedPromise (f a)
    funMap f BrokenPromise       =  BrokenPromise
