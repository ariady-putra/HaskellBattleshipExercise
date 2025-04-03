module IfShipsAreNotKnown where

import Control.Monad (forM_)
import Control.Monad.Trans.State.Lazy (State, execState, get, put)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import System.IO (stdin, stdout)
import System.Random (randomRIO)

data Tile
  = Tile
  { isShip :: Bool,
    isShot :: Bool
  }
  deriving (Eq)

instance Show Tile where
  show (Tile False False) = "_" -- Unknown
  show (Tile False True) = "w" -- Water
  show (Tile True _) = "X" -- Ship

type ShotHistory = [(Int, Int, ShootResult)]

data Direction
  = North
  | South
  | West
  | East
  deriving (Eq, Show)

data ShootStrategy = Random | Try [(Direction, Int, Int)] deriving (Eq, Show)

data Board
  = Board
  { tiles :: [[Tile]],
    ships :: [Int],
    shotHistory :: ShotHistory,
    currShotResult :: ShootResult,
    nextShotStrategy :: ShootStrategy
  }
  deriving (Eq, Show)

mkBoard n m ships =
  Board
    { tiles = replicate n $ replicate m (Tile False False),
      ships = ships,
      shotHistory = [],
      currShotResult = Miss,
      nextShotStrategy = Random
    }

data ShootResult = Hit | Miss | HitSunk deriving (Eq, Show)

updateTiles :: Int -> Int -> ShootResult -> [[Tile]] -> [[Tile]]
updateTiles x y currShotResult tiles =
  let h = length tiles
   in [updateRow n | n <- [0 .. h], n < h]
  where
    updateRow n =
      let w = length $ tiles !! n
       in [ if (n, m) == (x, y)
              then Tile {isShip = currShotResult /= Miss, isShot = True}
              else tiles !! n !! m
            | m <- [0 .. w],
              m < w
          ]

testUpdateTiles =
  forM_
    [Hit, Miss, HitSunk]
    (\shot -> putStrLn "" >> mapM_ print (updateTiles 1 2 shot $ tiles $ mkBoard 3 4 [1 .. 2]))

type ShootFn = Int -> Int -> State Board ShootResult

calcTryCoordinates :: (Int, Int) -> [[Tile]] -> ShotHistory -> ShootStrategy
calcTryCoordinates (x, y) tiles history =
  let w = length $ tiles !! x
      h = length tiles
      ns n coordinates
        | n < 0 || isShip (tiles !! n !! y) || any (\(i, j, _) -> i == n && j == y) history = coordinates
        | otherwise = (North, n, y) : ns (n - 1) coordinates
      ss n coordinates
        | n >= h || isShip (tiles !! n !! y) || any (\(i, j, _) -> i == n && j == y) history = coordinates
        | otherwise = (South, n, y) : ss (n + 1) coordinates
      ws m coordinates
        | m < 0 || isShip (tiles !! x !! m) || any (\(i, j, _) -> i == x && j == m) history = coordinates
        | otherwise = (West, x, m) : ws (m - 1) coordinates
      es m coordinates
        | m >= w || isShip (tiles !! x !! m) || any (\(i, j, _) -> i == x && j == m) history = coordinates
        | otherwise = (East, x, m) : es (m + 1) coordinates
      nCoordinates = ns (x - 1) []
      sCoordinates = ss (x + 1) []
      wCoordinates = ws (y - 1) []
      eCoordinates = es (y + 1) []
      coordinates = nCoordinates ++ sCoordinates ++ wCoordinates ++ eCoordinates
   in Try coordinates

testCalcTryCoordinates = do
  let toTiles = map (\t -> Tile (t == 'X') False)
  let tiles =
        map
          toTiles
          [ "XXXX_",
            "_____",
            "X_XX_",
            "X____",
            "X_X__"
          ]
  let Try coordinates = calcTryCoordinates (2, 2) tiles [(2, 3, Miss)]
  mapM_ print coordinates

shoot :: ShootFn
shoot x y = do
  (Board tiles ships shotHistory currShotResult nextShotStrategy) <- get

  let tiles' = updateTiles x y currShotResult tiles

  -- TODO: ships' = if HitSunk then deduce which ship to pop from the list
  let ships' = ships

  let shotHistory' = shotHistory ++ [(x, y, currShotResult)]

  let nextShotStrategy' =
        case nextShotStrategy of
          Try (_ : next) ->
            if currShotResult == Hit
              then Try next
              else
                let (stopDirection, _, _) = head next
                 in Try (filter (\(currDirection, _, _) -> currDirection /= stopDirection) next)
          _ ->
            if currShotResult == Hit
              then calcTryCoordinates (x, y) tiles shotHistory'
              else Random

  put $ Board tiles' ships' shotHistory' currShotResult nextShotStrategy'

  return currShotResult

nextShot :: Int -> Int -> ShotHistory -> ShootStrategy -> IO (Int, Int)
nextShot n m history strategy =
  let doRandomShot = do
        x <- randomRIO (0, n - 1)
        y <- randomRIO (0, m - 1)
        if any (\(i, j, _) -> i == x && j == y) history
          then nextShot n m history strategy
          else return (x, y)
   in case strategy of
        Try ((_direction, x, y) : _) -> return (x, y)
        _ -> doRandomShot

askShotStatus :: Int -> Int -> IO ShootResult
askShotStatus x y = do
  let row = x + 1
  let col = y + 1
  putStrLn $ "I shot (row:" ++ show row ++ ", col:" ++ show col ++ ") what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]"
  c <- getChar
  putStrLn ""
  case c of
    '0' -> return Miss
    '1' -> return Hit
    '2' -> return HitSunk
    _ -> do
      putStr "Invalid input! Valid inputs are 0 - Miss, 1 - Hit, 2 - HitSunk."
      putStr " "
      askShotStatus x y

draw :: Board -> IO ()
draw board = mapM_ print $ tiles board

play :: Int -> Int -> ShootFn -> Board -> IO ()
play n m shoot board = do
  let (Board tiles ships shotHistory currShotResult nextShotStrategy) = board
  case ships of
    [] -> do
      putStrLn "I should have sunk all your ships:"
      mapM_ print shotHistory
      putStrLn "GAME OVER"
    _ -> do
      (x, y) <- nextShot n m shotHistory nextShotStrategy
      currShotResult' <- askShotStatus x y
      let board' = execState (shoot x y) board {currShotResult = currShotResult'}
      draw board'
      play n m shoot board'

sinkAllShips :: Int -> Int -> [Int] -> ShootFn -> IO ()
sinkAllShips n m shipLengths shoot = do
  let board = mkBoard n m shipLengths
  play n m shoot board

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  putStrLn "+------------------------------------+"
  putStrLn "| Welcome to Haskell-Battleship Game |"
  putStrLn "+------------------------------------+"
  putStrLn ""
  putStrLn "TODO: Prompt user for board configurations"

  -- For example, 5x5 board with 1..4 ships with this secret arrangement:
  -- Col  : 1 2 3 4 5
  -- Row 1: X X X X _
  -- Row 2: _ _ _ _ _
  -- Row 3: X _ X X _
  -- Row 4: X _ _ _ _
  -- Row 5: X _ X _ _
  sinkAllShips 5 5 [1 .. 4] shoot
