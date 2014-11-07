-- y h = h (y h)

data Mu f = In (f (Mu f))
