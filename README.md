A very thin wrapper around [Termbox](github.com/nsf/termbox).

Got the idea to do this from [Rustbox](github.com/gchp/rustbox), as you can
probably tell from the following example.

Example
-------

An excerpt from `Example.hs`.

```haskell
import Termbox
import Termbox.Enums
import Termbox.Modes

main :: IO ()
main = do
    (Right _) <- tbInit
    tbSelectInputMode inputMode { isEsc = True, isMouse = True }
    puts 1 1 White Black "Hello, world!"
    puts 1 3 White Black "Press 'q' to quit."
    tbPresent
    loop
    tbShutdown
  where
    loop = do
      e <- tbPollEvent
      puts 1 8 Magenta Black (show e) >> tbPresent
      case e of
        Right (KeyEvent _ _ 113) -> return ()
        _                        -> loop
```


Build
-----

Get [Termbox](github.com/nsf/termbox).

    $ git clone github.com/nsf/termbox .termbox

Build it.

    $ cd termbox
    $ ./waf configure --prefix=/usr
    $ ./waf
    $ ./waf install --targets=termbox_static --destdir=$DEST

Now we're ready to build the bindings.

    $ TB_INCLUDE_DIR=`pwd`/.termbox/src TB_LIB_DIR=`pwd`/.termbox/dist/lib cabal build

And run the example.

    $ DYLD_LIBRARY_PATH=.termbox/dist/lib dist/build/example/example
