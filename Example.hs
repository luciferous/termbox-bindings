module Main where

import Control.Monad (zipWithM_, void)
import Termbox
import Termbox.Enums

import Prelude hiding (init)

putc :: Int -> Int -> Color -> Color -> Char -> IO ()
putc x y fg bg c =
    changeCell x y (fromIntegral . fromEnum $ c)
                   (fromIntegral . fromEnum $ fg)
                   (fromIntegral . fromEnum $ bg)

puts :: Int -> Int -> Color -> Color -> String -> IO ()
puts x y fg bg s =
    zipWithM_ (\x' c -> putc x' y fg bg c) [x..] s

main :: IO ()
main = do
    init >>= either ((shutdown >>) . putStrLn) (void . return)
    setInputMode inputMode { isEsc = True, isMouse = True }
    puts 1 1 White Black "Hello, world!"
    puts 1 3 White Black "Press 'q' to quit."
    im <- getInputMode
    om <- getOutputMode
    puts 1 5 Magenta Black $ "Current input mode: " ++ (show im)
    puts 1 6 Magenta Black $ "Current output mode: " ++ (show om)
    present
    loop
    shutdown
  where
    loop = do
      e <- pollEvent
      puts 1 8 Magenta Black (show e)
      present
      case e of
        Left msg                 -> error msg
        Right (KeyEvent _ _ 113) -> return ()
        _                        -> loop
