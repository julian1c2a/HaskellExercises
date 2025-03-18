{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_HaskellExercises (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "HaskellExercises"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A simple Haskell project"
copyright :: String
copyright = "2023 Julia"
homepage :: String
homepage = "https://github.com/githubuser/HaskellExercises#readme"
