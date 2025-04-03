module IfShipsAreKnown where

import Control.Monad.Trans.State.Lazy (State, execState, get, put)

type IsHit = Bool

newtype Ship = Ship {getCoordinates :: [(Int, Int, IsHit)]} deriving (Show)

data Board
  = Board
  { getN :: Int,
    getM :: Int,
    getShips :: [Ship],
    getShotsHistory :: [(Int, Int, ShootResult)]
  }
  deriving (Show)

data ShootResult = Hit | Miss | HitSunk deriving (Eq, Show)

-- instance Show ShootResult where
--   show Hit = "1"
--   show Miss = "0"
--   show HitSunk = "2"

type ShootFn = Int -> Int -> State Board ShootResult

shoot :: ShootFn
shoot x y = do
  board <- get
  let ships = getShips board
  let shots = getShotsHistory board

  -- update ships
  let ships' =
        map
          ( Ship
              . map
                ( \coordinate@(i, j, _) ->
                    if (i, j) == (x, y)
                      then (i, j, True) -- mark IsHit == True when (i,j) == (x,y)
                      else coordinate -- otherwise, return the original state
                )
              . getCoordinates
          )
          ships

  -- check shot
  let shotResult
        | sunk `any` ships' = HitSunk
        | hit `any` ships' = Hit
        | otherwise = Miss
        where
          isSunk ship = all (\(_, _, isHit) -> isHit) $ getCoordinates ship
          sunk ship = any (\(i, j, _) -> (i, j) == (x, y) && isSunk ship) $ getCoordinates ship
          hit ship = any (\(i, j, _) -> i == x && j == y) $ getCoordinates ship

  -- update board
  put board {getShips = ships', getShotsHistory = shots ++ [(x, y, shotResult)]}

  return shotResult

testShoot x y = execState (shoot x y)

testBoard = Board 5 5 [Ship [(0, 0, False), (0, 1, False)], Ship [(2, 2, False), (3, 2, False)]] []

sinkAllShips :: Int -> Int -> [Int] -> ShootFn -> IO ()
sinkAllShips n m shipLengths shoot = do
  let initBoard = Board n m [Ship [(0, 0, False), (0, 1, False)], Ship [(2, 2, False), (3, 2, False)]] []

  let play x y board
        | x == n = mapM_ print $ getShotsHistory board
        | y == m = play (x + 1) 0 board
        | otherwise = do
            let board' = execState (shoot x y) board
            play x (y + 1) board'
  play 0 0 initBoard

  return ()

todoShipLengths = []

testSinkAllShips = sinkAllShips 5 5 todoShipLengths shoot
