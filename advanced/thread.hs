-- http://www.haskellforall.com/2012/09/concurrency-lists-of-kleisli-arrows.html

data Step a = YieldL
            | Step a
            | YieldR

type Thread a = [Step a]

(<>) :: Thread a -> Thread a -> Thread a
[]  <> ys  = []
(YieldL:xs) <> ys = YieldL:(xs <> ys)
(Step a:xs) <> ys = Step a:(xs <> ys)
(YieldR:xs) <> (YieldL:ys) = (xs <> ys)
-- From this point onward, xs = YieldR:xs'
xs <> (Step a:ys) = Step a:(xs <> ys)
xs <> (YieldR:ys) = YieldR:(xs <> ys)
xs <> []  = []
-- Interestingly, this new operation is associative:
--(xs <> ys) <> zs = xs <> (ys <> zs)

-- empty thread which acts like an identity:
mempty = YieldR:YieldL:mempty
--mempty <> xs = xs
--xs <> mempty = xs
