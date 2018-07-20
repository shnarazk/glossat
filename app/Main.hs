module Main where

--import Lib
import Graphics.Gloss
import Prelude                                  hiding ( lines )
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, MVar(..)
                          , newEmptyMVar, takeMVar)
import Control.Exception
import Control.Monad ((<=<), unless, void, when)
import Data.IORef
import System.Environment
import System.IO.Unsafe
import System.Random
import SAT.Mios

getRand :: Float -> Float
getRand _ = unsafePerformIO $ getStdRandom (randomR (30, 100))

dump :: MVar [Int]
dump = unsafePerformIO newEmptyMVar

getSolverData :: Float -> [Int]
getSolverData _ = unsafePerformIO $ takeMVar dump

xp :: Float -> IORef Float
xp t = unsafePerformIO $ newIORef 20

main :: IO ()
main = do
  (f:_) <- (++ ["/home/narazaki/Repositories/SATbench/sudoku/sudoku16.cnf"]) <$> getArgs
  void $ forkIO $ animate (InWindow f (800, 500) (20, 20)) black frame
  executeSolverSlicedOn dump f
  threadDelay 12000000
  return ()

frame :: Float -> Picture
frame time = Color white $ Scale 1.8 1.8 $ Translate 0 (-70) $ makeGrid (getSolverData time)

num2pos :: Int -> (Float, Float)
num2pos n = (fromIntegral (mod n 100) * 4 - 200, 150 - fromIntegral (div n 100) * 4 - 20)

makeGrid :: [Int] -> Picture
makeGrid l = Pictures $ title : zipWith light l [1 .. ]
  where
    title = Translate (-200) 160 $ Scale 0.07 0.07 $ Text "searching..."
    light :: Int -> Int -> Picture
    light state n
      | state == -1 = Color blue        $ Translate x y $ circleSolid 1.7
      | state ==  0 = color (greyN 0.2) $ Translate x y $ circleSolid 1.4
      | state ==  1 = Color white       $ Translate x y $ circleSolid 1.7
      where (x, y) = num2pos n

clockFractal :: Int -> Float -> Picture
clockFractal 0 _        = Blank
clockFractal n s        = Pictures [circ1, circ2, circ3, lines]
 where
        -- y offset from origin to center of circle 1.
        a       = 1 / sin (2 * pi / 6)
        -- x offset from origin to center of circles 2 and 3.
        b       = a * cos (2 * pi / 6)
        nf      = fromIntegral n
        rot     = if n `mod` 2 == 0
                        then   50 * s * (log (1 + nf))
                        else (-50 * s * (log (1 + nf)))
        -- each element contains a copy of the (n-1) iteration contained
        --      within a larger circle, and some text showing the time since
        --      the animation started.
        --
        circNm1
         = Pictures
                [ circle 1
                , Scale (a/2.5) (a/2.5) $ clockFractal (n-1) s
                , if n > 2
                    then Color cyan
                                $ Translate (-0.15) 1
                                $ Scale 0.001 0.001
                                $ Text (show s)
                    else Blank
                ]

        circ1   = Translate 0 a         $ Rotate rot    circNm1
        circ2   = Translate 1 (-b)      $ Rotate (-rot) circNm1
        circ3   = Translate (-1) (-b)   $ Rotate rot    circNm1

        -- join each iteration to the origin with some lines.
        lines
         = Pictures
                [ Line [(0, 0), ( 0,  a)]
                , Line [(0, 0), ( 1, -b)]
                , Line [(0, 0), (-1, -b)] ]
