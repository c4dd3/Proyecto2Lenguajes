{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Proyecto2Lenguajes (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Proyecto2Lenguajes"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Proyecto de ejemplo con cryptonite"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
