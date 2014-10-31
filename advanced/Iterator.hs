-- http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html

data Iterator a = a :< (Iterator a)
infixr 5 :<

initialHistory :: Iterator String
initialHistory = "" :< initialHistory

extract :: Iterator a -> a
extract (cmd :< _) = cmd

next :: Iterator a -> a
next (_ :< (cmd :< _)) = cmd

next' :: Iterator a -> Iterator a
next' (_ :< iterator) = iterator

(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

extend :: (Iterator a -> a) -> Iterator a -> Iterator a
extend f (a :< as) = (f (a :< as)) :< (extend f as)

exampleHistory :: Iterator String
exampleHistory =
       "^D"
    :< "^C"
    :< "eat flaming death"
    :< "hello?"
    :< "bye"
    :< "exit"
    :< "quit"
    :< "?"
    :< "help"
    :< "ed"
    :< initialHistory
