module Main where

import Control.Monad (zipWithM_)
import Termbox
import Termbox.Enums
import Termbox.Modes

putc :: Int -> Int -> Color -> Color -> Char -> IO ()
putc x y fg bg c =
    tbChangeCell x y (fromIntegral . fromEnum $ c)
                     (fromIntegral . fromEnum $ fg)
                     (fromIntegral . fromEnum $ bg)

puts :: Int -> Int -> Color -> Color -> String -> IO ()
puts x y fg bg s =
    zipWithM_ (\x' c -> putc x' y fg bg c) [x..] s

main :: IO ()
main = do
    (Right _) <- tbInit
    tbSelectInputMode inputMode { isEsc = True, isMouse = True }
    puts 1 1 White Black "Hello, world!"
    puts 1 3 White Black "Press 'q' to quit."
    im <- tbInputMode
    om <- tbOutputMode
    puts 1 5 Magenta Black $ "Current input mode: " ++ (show im)
    puts 1 6 Magenta Black $ "Current output mode: " ++ (show om)
    tbPresent
    loop
    tbShutdown
  where
    loop = do
      e <- tbPollEvent
      puts 1 8 Magenta Black (show e)
      tbPresent
      case e of
        Left msg                 -> error msg
        Right (KeyEvent _ _ 113) -> return ()
        _                        -> loop
