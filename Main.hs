{-# LANGUAGE FlexibleContexts #-}
module Main where


import Commands (Command(..), Action(..), Setting(..), Update(..), command)
import Game (Game, defaultGame, settings, state, GameSettings(..), GameState(..), setPlayerSnippets, setPlayerHasWeapon, setPlayerIsParalyzed, setGameField)
import GHC.IO.Handle (hSetBuffering, BufferMode(NoBuffering))
import GHC.IO.Handle.FD (stdout)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (MonadState(get), execStateT, modify)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as B (getContents, lines, toStrict)
import Move (move)


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  ls <- (map B.toStrict . B.lines) <$> B.getContents
  void $ execStateT (mapM_ processLine ls) defaultGame
  where
    processLine l =
      let
        res = parseOnly command l
      in
        case res of
          Left err ->
            error err
          Right cmd ->
            processCommand cmd


processCommand :: (MonadIO m, MonadState Game m) => Command -> m ()
processCommand (SettingsCommand (Timebank t)) =
  modify $ settings $ \s -> s { settingsTimebank = t }
processCommand (SettingsCommand (TimePerMove t)) =
  modify $ settings $ \s -> s { settingsTimePerMove = t }
processCommand (SettingsCommand (PlayerNames names)) =
  modify $ settings $ \s -> s { settingsPlayerNames = names }
processCommand (SettingsCommand (YourBot name)) =
  modify $ settings $ \s -> s { settingsYourBot = name }
processCommand (SettingsCommand (YourBotId i)) =
  modify $ settings $ \s -> s { settingsYourBotId = i }
processCommand (SettingsCommand (FieldWidth i)) =
  modify $ settings $ \s -> s { settingsFieldWidth = i }
processCommand (SettingsCommand (FieldHeight i)) =
  modify $ settings $ \s -> s { settingsFieldHeight = i }
processCommand (SettingsCommand (MaxRounds i)) =
  modify $ settings $ \s -> s { settingsMaxRounds = i }
processCommand (UpdateCommand (GameRound i)) =
  modify $ state $ \s -> s { stateRound = i }
processCommand (UpdateCommand (PlayerSnippets name i)) =
  modify $ state $ setPlayerSnippets name i
processCommand (UpdateCommand (PlayerHasWeapon name b)) =
  modify $ state $ setPlayerHasWeapon name b
processCommand (UpdateCommand (PlayerIsParalyzed name b)) =
  modify $ state $ setPlayerIsParalyzed name b
processCommand (UpdateCommand (GameField field)) =
  modify $ setGameField field
processCommand (ActionCommand (Move i)) = do
  g <- get
  m <- move i g
  liftIO $ print m
processCommand _ = do
  return ()
