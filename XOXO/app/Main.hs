import Graphics.Gloss

import Graphics.Gloss.Data.Color

import Graphics.Gloss.Data.Display

import Graphics.Gloss.Data.Picture

-- import Graphics.Gloss.Interface.Pure.Game

import Graphics.Gloss.Game

import qualified Data.Map as M
import Data.IORef

-- import Graphics.Gloss.Juicy
type Board = M.Map (Int, Int) Player

data Player = X | O | BlankP deriving Eq

data GameState = StartUp | Running | GameOver | WonX | WonO deriving Eq

data State = State { gamestate :: GameState
                   , boardDim  :: Int
                   , board     :: Board
                   , gridDim   :: Int
                   }

main :: IO ()
main = let size       = (600,600)
           window     =  (InWindow "Tic-Tac-Toe" size (300, 300))
           background = black
           fps        = 15
       in Graphics.Gloss.Game.play  
          window
          background
          fps
          initState
          render 
          nextEvent 
          []

render :: State -> Picture
render state = let currstate = gamestate state
               in case currstate of
                  StartUp -> startScreen (gridDim state, gridDim state)
                  GameOver -> gameoverScreen (gridDim state, gridDim state)
                  WonX    ->  wonXScreen (gridDim state, gridDim state)
                  WonO    ->  wonOScreen (gridDim state, gridDim state)
                  Running -> renderTable state

picStart = png "img/start.png"
picGameover = png "img/gameover.png"
picWonX = png "img/wonx.png"
picWonO = png "img/wono.png"

gameoverScreen windowSize = useImage picGameover windowSize
startScreen windowSize = useImage picStart windowSize
wonXScreen windowSize = useImage picWonX windowSize
wonOScreen windowSize = useImage picWonO windowSize

useImage :: Picture -> (Int,Int) -> Picture
useImage picture windowSize =
  let (_, (picWidth, picHeight)) = boundingBox picture
      (winWidth,winHeight)     = (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize)
      scaleW                   = winWidth / picWidth
      scaleH                   = winHeight / picHeight
      scaleFactor              = max scaleH scaleW
  in scale scaleFactor scaleFactor $ picture


initState = State { gamestate = StartUp
                  , boardDim  = 3
                  , board     = M.empty
                  , gridDim   = 540
                  }

movesOnBoard :: State -> [(Int, Int)]
movesOnBoard state = [(x, y) | x <- [0..(boardDim state)-1], y <- [0..(boardDim state)-1], getPlayer (board state) (x, y) == BlankP]
    
cellCoord:: (Float,Float) -> State -> (Int,Int)
cellCoord (x,y) state = (floor (( y + (fromIntegral (gridDim state) * 0.5)) / (cellSize state)), floor ((x + (fromIntegral (gridDim state) * 0.5)) / (cellSize state)))

calculatePlayerTurnFromState pos state = if getPlayer (board state) (cellCoord pos state) == BlankP && currPlayer state == X
                                              then M.insert (cellCoord pos state) X (board state)
                                         else if getPlayer (board state) (cellCoord pos state) == BlankP && currPlayer state == O
                                              then M.insert (cellCoord pos state) O (board state)
                                        else board state

currPlayer :: State -> Player
currPlayer state = if mod (boardDim state) 2 == 1 then if odd $ length $ movesOnBoard state  then  X else O
                  else if odd $ length $ movesOnBoard state  then  O else X

currGameState state | (winnerFromState state) == Just X = WonX
                    | (winnerFromState state) == Just O = WonO
                    | length (movesOnBoard state) == 0 = GameOver
                    | (gamestate state) == StartUp = StartUp
                    | otherwise =  Running

nextEvent :: Event -> State -> State
nextEvent (EventKey (MouseButton LeftButton) Up _ pos) state = if (currGameState state) == Running then state {gamestate = Running, board = calculatePlayerTurnFromState pos state}
                                                              else if (currGameState state) == WonX  then state {gamestate = WonX, board = M.empty}
                                                              else if (currGameState state) == WonO  then state {gamestate = WonO, board = M.empty}
                                                              else if (currGameState state) == GameOver then state {gamestate = GameOver}
                                                              else state

nextEvent (EventKey (SpecialKey KeySpace) Down _ _) state = if (currGameState state) == StartUp then state {gamestate = Running, boardDim = 3}
                                                              -- else if (currGameState state) == WonX then state {gamestate = Running, board = M.empty}
                                                              -- else if (currGameState state) == WonO then state {gamestate = Running, board = M.empty}
                                                              else if (currGameState state) == GameOver then state {gamestate = Running, board = M.empty}
                                                              else state

nextEvent (EventKey (Char '3') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 3 } else state
nextEvent (EventKey (Char '4') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 4 } else state
nextEvent (EventKey (Char '5') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 5 } else state
nextEvent (EventKey (Char '6') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 6 } else state
nextEvent (EventKey (Char '7') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 7 } else state
nextEvent (EventKey (Char '8') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 8 } else state
nextEvent (EventKey (Char '9') Down _ _) state = if currGameState state == StartUp then state { gamestate = Running, boardDim = 9 } else state
nextEvent _ state = state

--nova implementacija

cellSize :: State -> Float
cellSize state = fromIntegral (gridDim state) / fromIntegral (boardDim state)

winnerFromState :: State -> Maybe Player
winnerFromState state = getMaybeValue .map head .filter (\xs -> head xs /= BlankP && allSame xs) . map (map $ getPlayer (board state)) $ possibleMove (boardDim state, boardDim state) (boardDim state)

outcomeColorFromState :: State -> Color
outcomeColorFromState state = if winnerFromState state == Just X then red 
                              else if winnerFromState state == Just O then blue
                              else white

renderGridFromState :: State ->Picture
renderGridFromState state = color (outcomeColorFromState state) $ pictures
    $ concatMap (\i -> [ line [ (i * (cellSize state), 0)
                              , (i * (cellSize state), fromIntegral (gridDim state))
                              ]
                       , line [ (0, i * (cellSize state))
                              , (fromIntegral (gridDim state), i * (cellSize state))
                              ]
                       ])
      [0 .. fromIntegral (boardDim state)]

renderTable :: State -> Picture
renderTable state = translate (fromIntegral (gridDim state) * (-0.5)) (fromIntegral (gridDim state) * (-0.5))
                    $ pictures 
                    $ [translate (fromIntegral y  * (cellSize state) + (cellSize state) * 0.5) (fromIntegral x  * (cellSize state) + (cellSize state) * 0.5)  $renderPlayers (boardDim state) (getPlayer (board state) (x, y)) | x <- [0..(boardDim state) - 1], y <- [0..(boardDim state) - 1] ] ++ [renderGridFromState state ] 



renderPlayers :: Int ->Player -> Picture
renderPlayers dim X = color red $ rotate 45 $ pictures [rectangleSolid (270 / fromIntegral dim) (45 / fromIntegral dim), rectangleSolid (45 / fromIntegral dim) (270 / fromIntegral dim)]
renderPlayers dim O = color blue $ thickCircle (105 / fromIntegral dim) (45 / fromIntegral dim)
renderPlayers dim _ = blank

step :: [(Int, Int) -> (Int, Int)]
step = [ (\(x, y) -> (x+1, y  ))
             , (\(x, y) -> (x  , y+1))
             , (\(x, y) -> (x+1, y+1))
             , (\(x, y) -> (x+1, y-1))
             ]

getMaybeValue (x:_) = Just x
getMaybeValue _ = Nothing

allSame []     = True
allSame (x:xs) = all (==x) xs

possibleMove (x, y) dim =
    filter (all (\(i, j) -> i >= 0 && j >= 0 && i < x && j < y))
    [take dim (iterate d (i, j)) | i <- [0..x-1], j <- [0..y-1], d <- step]

getPlayer :: Board -> (Int, Int) -> Player
getPlayer = flip $ M.findWithDefault BlankP
