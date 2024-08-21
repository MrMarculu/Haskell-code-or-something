import System.Random
import Control.Monad (replicateM)

generatePoint :: IO (Double, Double)
generatePoint = do
    x <- randomRIO (-1.0, 1.0)
    y <- randomRIO (-1.0, 1.0)
    return (x, y)

generateNPoints :: Int -> IO [(Double, Double)]
generateNPoints n = replicateM n generatePoint

monteCarlo :: [(Double, Double)] -> Double
monteCarlo lst = monteCarlo' lst 0 0 
  where
    monteCarlo' :: [(Double, Double)] -> Double -> Double -> Double
    monteCarlo' [] cp sp = pi' cp sp
    monteCarlo' ((x', y'):rest) cp sp 
        | (x' * x' + y' * y') <= 1 = monteCarlo' rest (cp + 1) (sp + 1)
        | otherwise = monteCarlo' rest cp (sp + 1)
  
    pi' :: Double -> Double -> Double
    pi' cp sp = 4 * (cp / sp)
    
main :: IO ()
main = do
    points <- generateNPoints 1000
    print $ monteCarlo points
