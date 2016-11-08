-- Note: I didnt write this, just copied from solutions
import Control.Monad.Random

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (ele, theRest)
  where
    ele = xs !! (n-1)
    theRest = take (n-1) xs ++ drop n xs

rnd_select :: (MonadRandom m, Eq a) => [a] -> Int -> m [a]
rnd_select [] _ = return []
rnd_select _ 0  = return []
rnd_select ys n = do
  rnd_index <- getRandomR (1, length ys)
  let (x, xs) = removeAt rnd_index ys
  xs' <- rnd_select xs (n-1)
  return (x:xs')
