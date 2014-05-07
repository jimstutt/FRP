
import Control.Monad
newtype MyZipList a = MyZipList [a] deriving (Show)

getBase :: MyZipList a -> [a]
getBase (MyZipList xs) = xs

instance Monad MyZipList where
  return x = MyZipList $ repeat x
  m >>= f  = MyZipList $ bind (getBase m) (fmap getBase f)
    where
      bind :: [a] -> (a -> [b]) -> [b]
      bind [] f     = []
      bind (x:xs) f = case f x of
                        [] -> []
                        y:_ -> y : bind xs (fmap tailOrNil f)
      tailOrNil :: [b] -> [b]
      tailOrNil []     = []
      tailOrNil (x:xs) = xs
