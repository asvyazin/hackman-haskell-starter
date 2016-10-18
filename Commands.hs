{-# LANGUAGE OverloadedStrings #-}
module Commands where


import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 (Parser, string, space, decimal, many1, letter_ascii, sepBy, char, digit, endOfLine, digit)
import Data.Char (ord)
import Data.Text (Text, pack)


data Command
  = SettingsCommand Setting
  | UpdateCommand Update
  | ActionCommand Action
  | EmptyCommand
  deriving (Show)


data Setting
  = Timebank Int
  | TimePerMove Int
  | PlayerNames [Text]
  | YourBot Text
  | YourBotId Int
  | FieldWidth Int
  | FieldHeight Int
  | MaxRounds Int
  deriving (Show)


data Update
  = GameRound Int
  | GameField [[FieldItem]]
  | PlayerSnippets Text Int
  | PlayerHasWeapon Text Bool
  | PlayerIsParalyzed Text Bool
  deriving (Show)


data Action
  = Move Int
  deriving (Show)


data FieldItem
  = Empty
  | Inaccessible
  | Player Int
  | Enemy
  | Weapon
  | CodeSnippet
  deriving (Show, Eq)


data MoveDirection
  = MovePass
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown


instance Show MoveDirection where
  show MovePass = "pass"
  show MoveLeft = "left"
  show MoveRight = "right"
  show MoveUp = "up"
  show MoveDown = "down"


commands :: Parser [Command]
commands =
  command `sepBy` endOfLine


command :: Parser Command
command =
  settingsCommand <|> updateCommand <|> actionCommand <|> emptyCommand
  where
    settingsCommand =
      SettingsCommand <$> (string "settings" *> space *> setting)
    updateCommand =
      UpdateCommand <$> (string "update" *> space *> update)
    actionCommand =
      ActionCommand <$> (string "action" *> space *> action)
    setting =
      timebank <|> timePerMove <|> playerNames <|> yourBot <|> yourBotId <|> fieldWidth <|> fieldHeight <|> maxRounds
    update =
      gameRound <|> gameField <|> playerSnippets <|> playerHasWeapon <|> playerIsParalyzed
    action =
      Move <$> (string "move" *> space *> decimal)
    timebank =
      Timebank <$> (string "timebank" *> space *> decimal)
    timePerMove =
      TimePerMove <$> (string "time_per_move" *> space *> decimal)
    playerNames =
      PlayerNames <$> (string "player_names" *> space *> playerNamesList)
    yourBot =
      YourBot <$> (string "your_bot" *> space *> playerName)
    yourBotId =
      YourBotId <$> (string "your_botid" *> space *> decimal)
    fieldWidth =
      FieldWidth <$> (string "field_width" *> space *> decimal)
    fieldHeight =
      FieldHeight <$> (string "field_height" *> space *> decimal)
    maxRounds =
      MaxRounds <$> (string "max_rounds" *> space *> decimal)
    gameRound =
      GameRound <$> (string "game round " *> decimal)
    gameField =
      GameField <$> (string "game field " *> (gameFieldItems `sepBy` char ','))
    playerSnippets = do
      p <- playerName
      _ <- string " snippets "
      i <- decimal
      return $ PlayerSnippets p i
    playerHasWeapon = do
      p <- playerName
      _ <- string " has_weapon "
      b <- boolean
      return $ PlayerHasWeapon p b
    playerIsParalyzed = do
      p <- playerName
      _ <- string " is_paralyzed "
      b <- boolean
      return $ PlayerIsParalyzed p b
    playerNamesList =
      playerName `sepBy` char ','
    playerName =
      pack <$> many1 (letter_ascii <|> digit)
    gameFieldItems =
      many1 gameFieldItem
    boolean =
      (string "true" *> return True) <|>
      (string "false" *> return False)
    gameFieldItem =
      fiEmpty <|> fiInaccessible <|> fiPlayer <|> fiEnemy <|> fiWeapon <|> fiCodeSnippet
    fiEmpty =
      char '.' *> return Empty
    fiInaccessible =
      char 'x' *> return Inaccessible
    fiPlayer =
      (Player . toInt) <$> digit
    fiEnemy =
      char 'E' *> return Enemy
    fiWeapon =
      char 'W' *> return Weapon
    fiCodeSnippet =
      char 'C' *> return CodeSnippet
    toInt c =
      ord c - ord '0'
    emptyCommand =
      string "" *> return EmptyCommand
