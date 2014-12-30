module MyMonad (
  MyMonad(..),
  monadAp,
  monadBindFlipped,
  monadCompose,
  monadComposeFlipped,
  monadFilter,
  monadFmap,
  monadFold,
  monadFoldEffects,
  monadFor,
  monadForEffects,
  monadForever,
  monadJoin,
  monadMap,
  monadMapEffects,
  monadReplicate,
  monadReplicateEffects,
  monadSequence,
  monadSequenceEffects,
  monadUnless,
  monadWhen) where

import MyApplicativeFunctor


{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Minimal complete definition: '>>=' and 'return'.

Instances of 'Monad' should satisfy the following laws:

> return a >>= k  ==  k a
> m >>= return  ==  m
> m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h

Instances of both 'Monad' and 'Functor' should additionally satisfy the law:

> fmap f xs  ==  xs >>= return . f

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}

class (MyApplicativeFunctor m) => MyMonad m where
  -- | Inject a value into the monadic type.
  monadReturn :: a -> m a

  -- | Sequentially compose two actions, passing any value produced
  -- by the first as an argument to the second.
  --
  -- >>=
  --
  -- the bind combinator can be defined as follows:
  -- m >>= g = join (fmap g m)
  monadBind :: m a -> (a -> m b) -> m b

  -- | Sequentially compose two actions, discarding any value produced
  -- by the first, like sequencing operators (such as the semicolon)
  -- in imperative languages.
  --
  -- >>
  monadThen :: m a -> m b -> m b
  x `monadThen` y = x `monadBind` \_ -> y

  --pure = monadReturn
  --fs `apply` xs = fs `monadBind` (\f -> xs `monadBind` (\x -> monadReturn (f x)))
  --funMap f fa = fa `monadBind` monadReturn . f


instance MyMonad Maybe where
  monadReturn x = Just x
  x `monadBind` f = case x of Nothing -> Nothing
                              Just y -> f y


instance MyMonad (Either e) where
  monadReturn = Right
  Left l `monadBind` _  = Left l
  Right r `monadBind` k = k r


instance MyMonad [] where
  monadReturn x = [x]
  m `monadBind` k = foldr ((++) . k) [] m
  m `monadThen` k = foldr ((++) . (\_ -> k)) [] m


instance MyMonad ((->) r) where
  monadReturn = const
  f `monadBind` k = \r -> k (f r) r


-- fmap applies a given function to every element in a container.
monadFmap :: MyMonad m => (a -> b) -> m a -> m b
f `monadFmap` x = x `monadBind` (monadReturn . f)

-- join takes a container of containers and flattens it into a single container.
monadJoin :: MyMonad m => m (m a) -> m a
monadJoin x = x `monadBind` id


-- =<<
-- | Same as '>>=', but with the arguments interchanged.
monadBindFlipped :: MyMonad m => (a -> m b) -> m a -> m b
f `monadBindFlipped` x = x `monadBind` f

-- (>=>) is the monad composition operator,
-- a close analogue of the function composition operator (.), only with flipped arguments.
-- | Left-to-right Kleisli composition of monads.
monadCompose :: MyMonad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f `monadCompose` g = \x -> f x `monadBind` g

-- (<=<)
-- The operation order of (f . g) is the same as (f' <=< g').
-- | Right-to-left Kleisli composition of monads. @('>=>')@, with the arguments flipped
monadComposeFlipped :: MyMonad m => (b -> m c) -> (a -> m b) -> (a -> m c)
monadComposeFlipped = flip monadCompose


-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
monadSequence :: MyMonad m => [m a] -> m [a]
monadSequence ms = foldr k (monadReturn []) ms where
                     k m m' = m           `monadBind` (\x ->
                              m'          `monadBind` (\xs ->
                              monadReturn (x:xs)))

-- | Evaluate each action in the sequence from left to right,
-- and ignore the results.
monadSequenceEffects :: MyMonad m => [m a] -> m ()
monadSequenceEffects ms = foldr monadThen (monadReturn ()) ms


-- | @'mapM' f@ is equivalent to @'sequence' . 'map' f@.
monadMap :: MyMonad m => (a -> m b) -> [a] -> m [b]
monadMap f as =  monadSequence (map f as)

-- | @'mapM_' f@ is equivalent to @'sequence_' . 'map' f@.
monadMapEffects :: MyMonad m => (a -> m b) -> [a] -> m ()
monadMapEffects f as =  monadSequenceEffects (map f as)


-- | This generalizes the list-based 'filter' function.
monadFilter :: MyMonad m => (a -> m Bool) -> [a] -> m [a]
monadFilter _ [] = monadReturn []
monadFilter p (x:xs) = p x              `monadBind` (\flg ->
                       monadFilter p xs `monadBind` (\ys ->
                       monadReturn (if flg then x:ys else ys)))


-- | 'forM' is 'mapM' with its arguments flipped
monadFor :: MyMonad m => [a] -> (a -> m b) -> m [b]
monadFor = flip monadMap

-- | 'forM_' is 'mapM_' with its arguments flipped
monadForEffects :: MyMonad m => [a] -> (a -> m b) -> m ()
monadForEffects = flip monadMapEffects

-- | @'forever' act@ repeats the action infinitely.
monadForever :: MyMonad m => m a -> m b
monadForever a = let a' = a `monadThen` a'
                 in a'

{- | The 'foldM' function is analogous to 'foldl', except that its result is
encapsulated in a monad. Note that 'foldM' works from left-to-right over
the list arguments. This could be an issue where @('>>')@ and the `folded
function' are not commutative.


>       foldM f a1 [x1, x2, ..., xm]

==

>       do
>         a2 <- f a1 x1
>         a3 <- f a2 x2
>         ...
>         f am xm

If right-to-left evaluation is required, the input list should be reversed.
-}

monadFold :: MyMonad m => (a -> b -> m a) -> a -> [b] -> m a
monadFold _ a []     = monadReturn a
monadFold f a (x:xs) = f a x `monadBind` \fax -> monadFold f fax xs

-- | Like 'foldM', but discards the result.
monadFoldEffects :: MyMonad m => (a -> b -> m a) -> a -> [b] -> m ()
monadFoldEffects f a xs = monadFold f a xs `monadThen` monadReturn ()


-- | @'replicateM' n act@ performs the action @n@ times,
-- gathering the results.
monadReplicate :: MyMonad m => Int -> m a -> m [a]
monadReplicate n x = monadSequence (replicate n x)

-- | Like 'replicateM', but discards the result.
monadReplicateEffects :: MyMonad m => Int -> m a -> m ()
monadReplicateEffects n x = monadSequenceEffects (replicate n x)


{- | Conditional execution of monadic expressions. For example,

>       when debug (putStr "Debugging\n")

will output the string @Debugging\\n@ if the Boolean value @debug@ is 'True',
and otherwise do nothing.
-}

monadWhen :: MyMonad m => Bool -> m () -> m ()
monadWhen p s = if p then s else monadReturn ()

-- | The reverse of 'when'.
monadUnless :: MyMonad m => Bool -> m () -> m ()
monadUnless p s = if p then monadReturn () else s


{- | In many situations, the 'liftM' operations can be replaced by uses of
'ap', which promotes function application.

>       return f `ap` x1 `ap` ... `ap` xn

is equivalent to

>       liftMn f x1 x2 ... xn

-}

monadAp :: MyMonad m => m (a -> b) -> m a -> m b
monadAp = liftM2 id where
  -- | Promote a function to a monad, scanning the monadic arguments from
  -- left to right.  For example,
  --
  -- >    liftM2 (+) [0,1] [0,2]      = [0,2,1,3]
  -- >    liftM2 (+) (Just 1) Nothing = Nothing
  --
  liftM2 :: MyMonad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
  liftM2 f m1 m2 = m1          `monadBind` (\x1 ->
                   m2          `monadBind` (\x2 ->
                   monadReturn (f x1 x2)))
