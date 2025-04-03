# HaskellBattleship

## How to Run

```bash
cabal build
cabal run
```

Or, alternatively:

```bash
cd app
cabal repl
```

```cabal
:set -package random
:set -package transformers
:l IfShipsAreNotKnown.hs
main
```

## Gameplay Example

### Intro

```
+------------------------------------+
| Welcome to Haskell-Battleship Game |
+------------------------------------+

Tell me the lengths of the ships! For example: 4 3 2 3
4 3 2 3

Tell me the size of the board! (N x M), for example: 5 5
5 5

Now, arrange the ships secretly. I'll give you 5 seconds, and then I'll start shooting!
5...
4...
3...
2...
1...

I shot (row:3, col:1) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]

```

### Mid Game

```
I shot (row:3, col:1) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]
1
[_,_,_,_,_]
[_,_,_,_,_]
[X,_,_,_,_]
[_,_,_,_,_]
[_,_,_,_,_]
I shot (row:2, col:1) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]
0
[_,_,_,_,_]
[w,_,_,_,_]
[X,_,_,_,_]
[_,_,_,_,_]
[_,_,_,_,_]
I shot (row:4, col:1) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]
1
[_,_,_,_,_]
[w,_,_,_,_]
[X,_,_,_,_]
[X,_,_,_,_]
[_,_,_,_,_]
I shot (row:5, col:1) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]
2
[_,_,_,_,_]
[w,_,_,_,_]
[X,_,_,_,_]
[X,_,_,_,_]
[X,_,_,_,_]
I shot (row:3, col:2) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]

```

### End Game

```
I shot (row:3, col:3) what is the status? [0 - Miss, 1 - Hit, 2 - HitSunk]
2
[X,X,X,X,_]
[w,_,_,w,_]
[X,w,X,X,_]
[X,_,w,w,w]
[X,w,X,X,X]
I should have sunk all your ships:
I shot (row:2, col:2) - Hit
I shot (row:1, col:1) - Miss
I shot (row:3, col:3) - Hit
I shot (row:4, col:4) - HitSunk
I shot (row:2, col:2) - Miss
I shot (row:0, col:0) - Hit
I shot (row:0, col:0) - Hit
I shot (row:0, col:0) - Hit
I shot (row:0, col:0) - HitSunk
I shot (row:4, col:4) - Hit
I shot (row:3, col:3) - Miss
I shot (row:4, col:4) - Miss
I shot (row:4, col:4) - Hit
I shot (row:4, col:4) - HitSunk
I shot (row:3, col:3) - Miss
I shot (row:2, col:2) - Hit
I shot (row:1, col:1) - Miss
I shot (row:3, col:3) - Miss
I shot (row:2, col:2) - HitSunk
GAME OVER
```
