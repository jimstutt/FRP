data Box = Box {name::String, vol :: Int} deriving (Show)

--mkLens ''Model
--  <$> mkLens ''Box
--  <*> mKLens
--  <*> mkLens

data Model = Model {n::Box, t::Box, s::Box} deriving (Show)

main = do
  let n = Box {name="nhlat", vol=1}
      t = Box {name="tropic", vol=2}
      s = Box {name="shlat", vol=1}
      m = Model {n=n,t=t,s=s}

  putStrLn $ getName s
  print $ getVol s
  putStrLn $ getName t
  print $ getVol t
  putStrLn $ getName n
  print $ getVol n
  print $ getName $ getN m
  print $ getVol $ getT m

getN,getT,getS :: Model -> Box
getN (Model n _ _) = n
getT (Model _ t _) = t
getS (Model _ _ s) = s

getName :: Box -> String
getName (Box name _)  = name
getVol :: Box -> Int
getVol (Box _ vol) = vol

