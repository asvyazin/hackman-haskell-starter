module Move where


import Commands (MoveDirection(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Game (Game, getCurrentPosition, getPossibleMoves)
import System.Random (getStdRandom, Random(randomR))


move :: MonadIO m => Int -> Game -> m MoveDirection
move _ g = do
  let
    pos = getCurrentPosition g
    moves = getPossibleMoves g pos
  chooseRandom moves


chooseRandom :: MonadIO m => [a] -> m a
chooseRandom l = do
  let
    n = length l
  i <- liftIO $ getStdRandom (randomR (0, (n - 1)))
  return $ l !! i
