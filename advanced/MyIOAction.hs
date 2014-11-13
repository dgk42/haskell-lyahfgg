module MyIOAction(
  MyIOAction(..),
  echo,
  get,
  put,
  run,
  seqio) where

import MyFunctor
import MyApplicativeFunctor
import MyMonad


-- http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect

data MyIOAction a = Return a
                  | Put String (MyIOAction a)
                  | Get (String -> MyIOAction a)

instance Show a => Show (MyIOAction a) where
  show io = go 0 0 io where
    go m n (Return a) = ind m "Return " ++ show a
    go m n (Put s io) = ind m "Put " ++ show s ++ " (\n" ++ go (m+2) n io ++ "\n" ++ ind m ")"
    go m n (Get g)    = let i = "$" ++ show n
                        in ind m "Get (" ++ i ++ " -> \n" ++ go (m+2) (n+1) (g i) ++ "\n" ++ ind m ")"
    ind m s = replicate m ' ' ++ s

put :: String -> MyIOAction ()
put s = Put s (Return ())
get :: MyIOAction String
get = Get (\s -> Return s)


-- fmap
mapio :: (a -> b) -> MyIOAction a -> MyIOAction b
mapio f (Return a) = Return (f a)
mapio f (Put s io) = Put s (mapio f io)
mapio f (Get g)    = Get (\s -> mapio f (g s))

instance MyFunctor MyIOAction where
  funMap = mapio


-- bind
seqio :: MyIOAction a -> (a -> MyIOAction b) -> MyIOAction b
seqio (Return a) f = f a
seqio (Put s io) f = Put s (seqio io f)
seqio (Get g) f    = Get (\s -> seqio (g s) f)

instance MyMonad MyIOAction where
  monadReturn = Return
  monadBind = seqio

echo :: MyIOAction ()
echo = get `seqio` put


-- apply
appio :: MyIOAction (a -> b) -> MyIOAction a -> MyIOAction b
a `appio` b = a      `seqio` (\f ->
              b      `seqio` (\x ->
              Return (f x)))

instance MyApplicativeFunctor MyIOAction where
  pure = monadReturn
  apply = appio


instance Functor MyIOAction where
  fmap = mapio

instance Monad MyIOAction where
  return = Return
  (>>=) = seqio

run :: MyIOAction a -> IO a
run (Return a) = return a
run (Put s io) = putStrLn s >> run io
run (Get g)    = getLine >>= \s -> run (g s)
