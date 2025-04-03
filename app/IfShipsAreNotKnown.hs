module IfShipsAreNotKnown (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.Trans.State.Lazy (State, execState, get, put)
import Data.Char (isDigit)
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

calcShipLength :: (Int, Int) -> [[Tile]] -> Int
calcShipLength (x, y) tiles =
  let w = length $ tiles !! x
      h = length tiles
      ns n count
        | n < 0 || (not . isShip) (tiles !! n !! y) = count
        | otherwise = 1 + ns (n - 1) count
      ss n count
        | n >= h || (not . isShip) (tiles !! n !! y) = count
        | otherwise = 1 + ss (n + 1) count
      ws m count
        | m < 0 || (not . isShip) (tiles !! x !! m) = count
        | otherwise = 1 + ws (m - 1) count
      es m count
        | m >= w || (not . isShip) (tiles !! x !! m) = count
        | otherwise = 1 + es (m + 1) count
      nCount = ns (x - 1) 0
      sCount = ss (x + 1) 0
      wCount = ws (y - 1) 0
      eCount = es (y + 1) 0
   in if isShip (tiles !! x !! y)
        then 1 + nCount + sCount + wCount + eCount
        else 0

testCalcShipLength x y = do
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
  print $ calcShipLength (x, y) tiles

removeShip :: Int -> [Int] -> [Int]
removeShip _ [] = []
removeShip shipLength (l : ls) =
  if l == shipLength
    then ls
    else l : removeShip shipLength ls

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
  let Try coordinates = calcTryCoordinates (2, 3) tiles [(4, 3, Miss)]
  mapM_ print coordinates

shoot :: ShootFn
shoot x y = do
  (Board tiles ships shotHistory currShotResult nextShotStrategy) <- get

  let tiles' = updateTiles x y currShotResult tiles

  let ships' =
        if currShotResult == HitSunk
          then removeShip (calcShipLength (x, y) tiles') ships
          else ships

  let shotHistory' = shotHistory ++ [(x, y, currShotResult)]

  let nextShotStrategy' =
        case (currShotResult, nextShotStrategy) of
          -- Hit? Then keep hitting
          (Hit, Try (_ : next)) -> Try next
          -- Hit, but no prediction yet? Then predict
          (Hit, _) -> calcTryCoordinates (x, y) tiles shotHistory'
          -- Not hit, but there are predictions? Then redirect shots
          (_, Try ((stopDirection, _, _) : next)) ->
            let coordinates =
                  filter
                    ( \(currDirection, _, _) ->
                        currDirection /= stopDirection
                    )
                    next
             in if null coordinates
                  then Random
                  else Try coordinates
          -- Otherwise, shoot randomly
          (_, _) -> Random

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
      mapM_
        ( putStrLn
            . ( \(x, y, result) ->
                  let row = x + 1
                      col = y + 1
                   in "I shot (row:" ++ show row ++ ", col:" ++ show col ++ ") - " ++ show result
              )
        )
        shotHistory
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

askShipLengths :: IO [Int]
askShipLengths = do
  putStrLn "Tell me the lengths of the ships! For example: 4 3 2 3"
  ships <- words <$> getLine
  putStrLn ""
  if all (\ship -> all (\f -> f ship) [all isDigit, not . all (== '0')]) ships
    then return $ map read ships
    else do
      putStr "Invalid input! Make sure there's no zero-length ship."
      putStr " "
      askShipLengths

askBoardSize :: [Int] -> IO (Int, Int)
askBoardSize shipLengths = do
  putStrLn "Tell me the size of the board! (N x M), for example: 5 5"
  size <- words <$> getLine
  putStrLn ""

  let longestShip = foldr max 0 shipLengths
  let shipCount = length shipLengths
  let minDimension = max longestShip shipCount

  if 2 == length size && all ((>= minDimension) . read) size
    then do
      let [x, y] = map read size
      return (x, y)
    else do
      putStr "Invalid input! Make sure the board can contain all ships."
      putStr " "
      askBoardSize shipLengths

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  putStrLn "+------------------------------------+"
  putStrLn "| Welcome to Haskell-Battleship Game |"
  putStrLn "+------------------------------------+"
  putStrLn ""

  shipLengths <- askShipLengths
  (n, m) <- askBoardSize shipLengths

  putStrLn "Now, arrange the ships secretly. I'll give you 5 seconds, and then I'll start shooting!"
  forM_
    (reverse [1 .. 5])
    ( \t -> do
        threadDelay 500000
        putStrLn $ show t ++ "..."
        threadDelay 500000
    )
  putStrLn ""

  -- For example, 5x5 board with 1..4 ships with this secret arrangement:
  -- Col  : 1 2 3 4 5
  -- Row 1: X X X X _
  -- Row 2: _ _ _ _ _
  -- Row 3: X _ X X _
  -- Row 4: X _ _ _ _
  -- Row 5: X _ X X X
  -- sinkAllShips 5 5 [4, 3, 2, 3] shoot

  sinkAllShips n m shipLengths shoot
