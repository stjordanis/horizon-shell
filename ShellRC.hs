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

import qualified Data.Aeson          as A
import qualified Data.Aeson.KeyMap   as A
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
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.List
import Data.Aeson.Lens as L
import qualified Data.Map as Map
import qualified Procex.Prelude      as P ()
import           Procex.Shell        (cd, initInteractive)
import qualified Procex.Shell        as P ()
import           System.Directory    (listDirectory, setCurrentDirectory)
import           System.Environment  (getEnv, setEnv)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import Network.HTTP.Simple
import qualified Text.Megaparsec.Char.Lexer as M

promptFunction :: [String] -> Int -> IO String
promptFunction _modules _line = do
  d <- getEnv "PWD"
  setCurrentDirectory d
  pure $ "\ESC[33m\STXHorizon: \ESC[m\STX"

_init :: IO ()
_init = do
  initInteractive
  getEnv "REALHOME" >>= setEnv "HOME"

type PVPVersion :: Type
newtype PVPVersion = MkPVPVersion { fromVersion :: [Int] }
  deriving (Show, Eq, Ord)



numbers = M.decimal `M.sepBy` M.char '.'

hackagePkg :: H.Name -> IO A.Value
hackagePkg (H.MkName x) = do
  k <- parseRequest $ "http://hackage.haskell.org/package/" <> T.unpack x
  getResponseBody <$> httpJSON k

getVersions :: A.Value -> [Text]
getVersions = Map.keys . Map.filter (\x -> x == A.String "normal") . A.toMapText . L.view L._Object

toPVPVersion :: Text -> Either (M.ParseErrorBundle Text Text) PVPVersion
toPVPVersion = fmap MkPVPVersion . M.parse numbers ""

pvpVersionToVersion :: PVPVersion -> H.Version
pvpVersionToVersion (MkPVPVersion u) = H.MkVersion $ T.intercalate "." (fmap (T.pack . show) u)

hackagePkgLatest :: H.Name -> IO H.Version
hackagePkgLatest x = do
  k <- hackagePkg x
  let j = fmap maximum . traverse toPVPVersion $ getVersions k
  pure $ case j of
            Right l -> pvpVersionToVersion l
            Left e -> error $ show e

_FromHackage :: L.Prism' H.HaskellSource H.HackageSource
_FromHackage =
  L.prism'
    H.FromHackage
    (\case 
        H.FromHackage y -> Just y
        _ -> Nothing
    )

nameL :: L.Lens' H.HackageSource H.Name
nameL = L.lens H.name (\x y -> x { H.name = y })

sourceL :: L.Lens' H.HaskellPackage H.HaskellSource
sourceL = L.lens H.source (\x y -> x { H.source = y })

_Hackages :: L.Traversal' H.HorizonExport H.HackageSource
_Hackages = H.packagesL . L.iso H.fromPackageList H.MkPackageList . L.itraversed . sourceL . _FromHackage
