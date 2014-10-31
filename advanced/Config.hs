module Config (
  (#),
  configBuilder,
  defaultConfig,
  extend,
  extract,
  goFaster,
  profile) where

-- http://www.haskellforall.com/2013/02/you-could-have-invented-comonads.html


type Option = String

data Config = MakeConfig [Option] deriving (Show, Eq)

-- We want to encapsulate the Config and prevent the user from
-- disassembling it, so we don't export the MakeConfig constructor to prevent
-- the user from pattern matching on the constructor.
-- Instead, we export a function in place of the constructor so that
-- the user can still create values of type Config

configBuilder :: [Option] -> Config
configBuilder = MakeConfig

defaultConfig :: [Option] -> Config
defaultConfig options = MakeConfig (["-Wall"] ++ options)

profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]

goFaster :: ([Option] -> Config) -> Config
goFaster builder = builder ["-O2"]

profile' :: ([Option] -> Config) -> [Option] -> Config
profile' builder = \options -> builder (["-prof", "-auto-all"] ++ options)

goFaster' :: ([Option] -> Config) -> [Option] -> Config
goFaster' builder = \options -> builder (["-O2"] ++ options)

-- Additionally, we need some sort of extract function that
-- completes the process and returns the final configuration.
-- That's easy since we can flush the builder by
-- supplying it with an empty option list
extract :: ([Option] -> Config) -> Config
extract builder = builder []

-- We would like to emulate the post-fix function application of
-- objected oriented programming, so we define the following infix operator
-- that simulates calling a method on an object
-- (&) is another popular name for this operator
(#) :: a -> (a -> b) -> b
x # f = f x
infixl 0 #

{-
>>> let builder1 = defaultConfig # profile'
>>> let builder2 = builder1 # goFaster'
>>> builder2 # extract
MakeConfig ["-Wall","-prof","-auto-all","-O2"]
-}

extend :: (([Option] -> Config) -> Config) -> ([Option] -> Config) -> ([Option] -> Config)
extend setter builder =
  \opts2 ->
    setter (\opts1 ->
      builder (opts1 ++ opts2))
