import Data.Maybe (fromJust)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Distribution.PackageDescription(extraLibDirs, library, libBuildInfo)
import System.Directory (getCurrentDirectory)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \meta flags -> do
      build <- confHook simpleUserHooks meta flags
      let pd  = localPkgDescr build
          lib = fromJust $ library pd
          info = libBuildInfo lib
          extras = extraLibDirs info
      pwd <- getCurrentDirectory
      return build
        { localPkgDescr = pd
          { library = Just $ lib
            { libBuildInfo = info
              { extraLibDirs = (pwd ++ "/.termbox/dist/lib") : extras }
            }
          }
        }
  }
