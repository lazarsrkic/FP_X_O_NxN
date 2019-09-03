import Graphics.Gloss (play, white, black, red, blue, Display(InWindow))

--import Graphics.Gloss
import qualified Data.Map as M
import Data.IORef
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate, blank, line)

import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), KeyState (Up), MouseButton (LeftButton),
     Key (MouseButton))

type Board = M.Map (Int, Int) Player

data Player = X | O | Blank deriving Eq


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
renderPlayer X = color red $ rotate 45 $ pictures [rectangleSolid 90 10, rectangleSolid 10 90]
renderPlayer O = color blue $ thickCircle 35 10
renderPlayer _ = blank

getPlayer :: Board -> (Int, Int) -> Player
getPlayer = flip $ M.findWithDefault Blank

renderGrid :: Picture
renderGrid =  pictures
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
                $   pictures $ [translate (fromIntegral y  * cellWidth + cellWidth * 0.5) (fromIntegral x  * cellHeight + cellHeight * 0.5)  $renderPlayer $ getPlayer b (x, y) | x <- [0..n], y <- [0..n] ] ++ [renderGrid] 
  

nextState (EventKey (MouseButton LeftButton) Up _ pos) b =
    if getPlayer b (toCellCoord pos) == Blank 
    then M.insert (toCellCoord pos) X b
    else b
nextState _ b = b    

currentPlayer :: Board -> Player
currentPlayer b = X
    
toCellCoord :: (Float, Float) -> (Int, Int)
toCellCoord (x, y) = (floor (( y + (fromIntegral height * 0.5)) / cellHeight), floor((x + (fromIntegral width * 0.5)) / cellWidth))                 
