{-# LANGUAGE OverloadedStrings #-}
module Game where


import Commands (FieldItem(..), MoveDirection(..))
import Data.Array (Array, listArray, assocs, (!))
import Data.List (find)
import Data.Map.Strict (Map, empty, insert)
import Data.Maybe (fromJust)
import Data.Text (Text)


data Game
  = Game
  { gameSettings :: GameSettings
  , gameState :: GameState
  } deriving (Show)


data GameSettings
  = GameSettings
  { settingsTimebank :: Int
  , settingsTimePerMove :: Int
  , settingsPlayerNames :: [Text]
  , settingsYourBot :: Text
  , settingsYourBotId :: Int
  , settingsFieldWidth :: Int
  , settingsFieldHeight :: Int
  , settingsMaxRounds :: Int
  } deriving (Show)


data GameState
  = GameState
  { stateRound :: Int
  , stateSnippets :: Map Text Int
  , stateHasWeapon :: Map Text Bool
  , stateIsParalyzed :: Map Text Bool
  , stateField :: Array (Int, Int) [FieldItem]
  } deriving (Show)


defaultGame :: Game
defaultGame =
  Game defaultSettings defaultState
  where
    defaultSettings =
      GameSettings 0 0 [] "" 0 0 0 0
    defaultState =
      GameState 0 empty empty empty (listArray ((0, 0), (0, 0)) [])


settings :: (GameSettings -> GameSettings) -> Game -> Game
settings f g =
  let
    newSettings = f (gameSettings g)
  in
    g { gameSettings = newSettings }


state :: (GameState -> GameState) -> Game -> Game
state f g =
  let
    newState = f (gameState g)
  in
    g { gameState = newState }


setPlayerSnippets :: Text -> Int -> GameState -> GameState
setPlayerSnippets name i s =
  let
    newSnippets = insert name i $ stateSnippets s
  in
    s { stateSnippets = newSnippets }


setPlayerHasWeapon :: Text -> Bool -> GameState -> GameState
setPlayerHasWeapon name b s =
  let
    newHasWeapon = insert name b $ stateHasWeapon s
  in
    s { stateHasWeapon = newHasWeapon }


setPlayerIsParalyzed :: Text -> Bool -> GameState -> GameState
setPlayerIsParalyzed name b s =
  let
    newIsParalyzed = insert name b $ stateIsParalyzed s
  in
    s { stateIsParalyzed = newIsParalyzed }


setGameField :: [[FieldItem]] -> Game -> Game
setGameField field g =
  let
    w =
      settingsFieldWidth $ gameSettings g
    h =
      settingsFieldHeight $ gameSettings g
    oldState =
      gameState g
    newState =
      oldState { stateField = listArray ((0, 0), (h - 1, w - 1)) field }
  in
    g { gameState = newState }


getCurrentPosition :: Game -> (Int, Int)
getCurrentPosition g =
  let
    botId =
      settingsYourBotId $ gameSettings g
    (pos, _) =
      fromJust $ find (\(_, fieldItems) -> any (== (Player botId)) fieldItems) $ assocs $ stateField $ gameState g
  in
    pos


isAccessiblePosition :: Game -> (Int, Int) -> Bool
isAccessiblePosition g pos =
  let
    field =
      stateField $ gameState g
    fieldItems =
      field ! pos
  in
    all (/= Inaccessible) fieldItems


getPossibleMoves :: Game -> (Int, Int) -> [MoveDirection]
getPossibleMoves g (y, x) =
  let
    possibleNeighbours =
      [ (MoveUp, (y - 1, x))
      , (MoveLeft, (y, x - 1))
      , (MoveDown, (y + 1, x))
      , (MoveRight, (y, x + 1))
      ]
    validNeighbours =
      filter (isValidPosition . snd) possibleNeighbours
  in
    map fst validNeighbours
  where
    isValidPosition p@(y', x') =
      let
        w =
          settingsFieldWidth $ gameSettings g
        h =
          settingsFieldHeight $ gameSettings g
      in
        (x' >= 0) && (x' < w) && (y' >= 0) && (y' < h) && isAccessiblePosition g p
