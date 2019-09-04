import Graphics.Gloss (play, white, black, red, blue, Display(InWindow))


import qualified Data.Map as M
import Data.IORef
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate, blank, line)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), KeyState (Up), MouseButton (LeftButton),
     Key (MouseButton))

type Board = M.Map (Int, Int) Player

data Player = X | O | Blank deriving Eq

data GameState = Running | GameOver  deriving Eq



height = 720
width = 480
n = 3

main :: IO ()
main = do
    play (InWindow "Tic-Tac-Toe" (height, width) (300, 300)) white 20 M.empty renderBoard nextState (const id)


cellWidth :: Float
cellWidth = fromIntegral width / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral height / fromIntegral  n

renderPlayer :: Player -> Picture
renderPlayer X = color red $ rotate 45 $ pictures [rectangleSolid 90 15, rectangleSolid 15 90]
renderPlayer O = color blue $ thickCircle 35 15
renderPlayer _ = blank

getPlayer :: Board -> (Int, Int) -> Player
getPlayer = flip $ M.findWithDefault Blank

renderGrid :: Board -> Picture
renderGrid b = color (outcomeColor b)  $ pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0)
                              , (i * cellWidth, fromIntegral height)
                              ]
                       , line [ (0, i * cellHeight)
                              , (fromIntegral width, i * cellHeight)
                              ]
                       ])
      [0 .. fromIntegral n]



renderBoard :: Board -> Picture
renderBoard b = translate (fromIntegral width * (-0.5))
                          (fromIntegral height * (-0.5)) 
                 $ pictures $ [translate (fromIntegral y  * cellWidth + cellWidth * 0.5) (fromIntegral x  * cellHeight + cellHeight * 0.5)  $renderPlayer $ getPlayer b (x, y) | x <- [0..n - 1], y <- [0..n - 1] ] ++ [renderGrid b ] 
  

nextState (EventKey (MouseButton LeftButton) Up _ pos) b = if currentGameState b == Running then  calculatePlayerTurn pos b
                                                                                          else M.empty 
nextState _ b = b    


calculatePlayerTurn pos b =  if getPlayer b (toCellCoord pos) == Blank && currentPlayer b == X
            then M.insert (toCellCoord pos) X b
    else if  getPlayer b (toCellCoord pos) == Blank && currentPlayer b == O
            then M.insert (toCellCoord pos) O b 

    else b 


currentPlayer :: Board -> Player
currentPlayer b = if odd $ length $ movesOnBoard b then   X
                  else  O
                  
movesOnBoard :: Board -> [(Int, Int)]
movesOnBoard b = [(x, y) | x <- [0..n-1], y <- [0..n-1], getPlayer b (x, y) == Blank]
    
toCellCoord :: (Float, Float) -> (Int, Int)
toCellCoord (x, y) = (floor (( y + (fromIntegral height * 0.5)) / cellHeight), floor((x + (fromIntegral width * 0.5)) / cellWidth))                 


currentGameState b   |  Just player <- winner b  =  GameOver
                     |length ( movesOnBoard b) == 0 =  GameOver
                     |otherwise =  Running
                   
                   


getMaybeValue (x:_) = Just x
getMaybeValue _ = Nothing

allSame []     = True
allSame (x:xs) = all (==x) xs

winner :: Board -> Maybe Player
winner b = getMaybeValue .map head .filter (\xs -> head xs /= Blank && allSame xs) . map (map $ getPlayer b) $ possibleMoves (n, n) 


step :: [(Int, Int) -> (Int, Int)]
step = [ (\(x, y) -> (x+1, y  ))
             , (\(x, y) -> (x  , y+1))
             , (\(x, y) -> (x+1, y+1))
             , (\(x, y) -> (x+1, y-1))
             ]

possibleMoves (x, y)  =
    filter (all (\(i, j) -> i >= 0 && j >= 0 && i < x && j < y))
    [take n (iterate d (i, j)) | i <- [0..x-1], j <- [0..y-1], d <- step]


outcomeColor b = if winner b == Just X  then red
                                     else if winner b == Just O then blue
                                                                else  black
