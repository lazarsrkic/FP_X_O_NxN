import Graphics.Gloss (play, white, black, Display(InWindow))

import Graphics.Gloss
import qualified Data.Map as M
import Data.IORef


type Board = M.Map (Int, Int) Marker

data Marker = X | O | Blank deriving Eq


height = 720
width = 480
n = 5

main :: IO ()
main = do
    play (InWindow "Tic-Tac-Toe" (height, width) (300, 300)) white 20 M.empty renderBoard (const id) (const id)


cellWidth :: Float
cellWidth = fromIntegral width / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral height / fromIntegral  n


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
                renderGrid     
               
