module Main where

--import Lib
import Graphics.Gloss
import Prelude                                  hiding ( lines )
import Control.Concurrent (forkIO, MVar(..), newEmptyMVar, putMVar, takeMVar)
import Control.Exception
import Control.Monad ((<=<), unless, void, when)
import Data.IORef
import System.Environment
import System.IO.Unsafe
import SAT.Mios
import SAT.Mios.OptionParser
import Sudoku16 (sudoku16)

terminated :: IORef (Bool, [Int])
terminated = unsafePerformIO $ newIORef (False, [])

{-# NOINLINE getSolverData #-}
getSolverData :: MVar [Int] -> Float -> [Int]
getSolverData mt _ = unsafePerformIO $ do
  t <- readIORef terminated
  if fst t
    then snd <$> readIORef terminated
    else do
      x <- takeMVar mt
      writeIORef terminated (False, x)
      return x

main :: IO ()
main = do
  args <- getArgs
  (title, conf) <- case args of
    []    -> return ("Sudoku16", miosDefaultOption { _targetFile = Right sudoku16 })
    (f:_) -> return (f,  miosDefaultOption { _targetFile = Left f })
  mutex <- newEmptyMVar :: IO (MVar [Int])
  void $ forkIO $ do
    executeSolverSliced mutex conf
    v <- readIORef terminated
    writeIORef terminated (True, snd v)
  animate (InWindow title (800, 500) (20, 20)) black (frame mutex)

{-# NOINLINE frame #-}
frame :: MVar [Int] ->  Float -> Picture
frame mt time = Color white $ Scale 1.8 1.8 $ Translate 0 (-70) $ makeGrid (getSolverData mt time)

num2pos :: Int -> (Float, Float)
num2pos n = (fromIntegral (mod n 100) * 4 - 200, 200 - fromIntegral (div n 100) * 4)

makeGrid :: [Int] -> Picture
makeGrid l = Pictures $ zipWith light l [0 .. ]
  where
    -- title = Translate (-200) 160 $ Scale 0.08 0.08 $ Text "searching..."
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
