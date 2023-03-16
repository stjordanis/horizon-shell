{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module ShellRC where

import qualified Control.Lens        as L
import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Pretty        as Dhall
import qualified Horizon.Spec.Lens   as H
import qualified Horizon.Spec.Pretty as H
import qualified Horizon.Spec.V0_10  as H
import qualified Procex.Prelude      as P ()
import           Procex.Shell        (cd, initInteractive)
import qualified Procex.Shell        as P ()
import           System.Directory    (listDirectory, setCurrentDirectory)
import           System.Environment  (getEnv, setEnv)

promptFunction :: [String] -> Int -> IO String
promptFunction _modules _line = do
  d <- getEnv "PWD"
  setCurrentDirectory d
  pure $ "\ESC[33m\STXHorizon: \ESC[m\STX"

_init :: IO ()
_init = do
  initInteractive
  getEnv "REALHOME" >>= setEnv "HOME"
