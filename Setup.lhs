#!/usr/bin/env runhaskell

> import Data.Maybe (fromJust)
> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo (localPkgDescr)
> import Distribution.PackageDescription
>   ( extraLibDirs
>   , includeDirs
>   , library
>   , libBuildInfo
>   )
> import System.Directory (getCurrentDirectory)
> import System.Environment (getEnv)
> 
> main :: IO ()
> main = do
>   tbIncludeDir <- getEnv "TB_INCLUDE_DIR"
>   tbLibDir <- getEnv "TB_LIB_DIR"
>   defaultMainWithHooks simpleUserHooks
>     { confHook = \meta flags -> do
>         build <- confHook simpleUserHooks meta flags
>         let pd  = localPkgDescr build
>             lib = fromJust $ library pd
>             info = libBuildInfo lib
>             extras = extraLibDirs info
>             includes = includeDirs info
>         pwd <- getCurrentDirectory
>         return build
>           { localPkgDescr = pd
>             { library = Just $ lib
>               { libBuildInfo = info
>                 { extraLibDirs = tbLibDir : extras
>                 , includeDirs = tbIncludeDir : includes
>                 }
>               }
>             }
>           }
>     }
