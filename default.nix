{ pkgs, haskellPackages, mkCrazyShell }:

let
  name = "horizon-shell";

  libraries = p: with p; [
    dhall
    http-conduit
    horizon-gen-nix
    horizon-spec
    horizon-spec-lens
    horizon-spec-pretty
    lens
    lens-aeson
    megaparsec
    path
    procex
    vector
  ];

  base-libraries = p: [
    p.bytestring
    p.text
  ];

  header = ''
    \ESC[33m\STX##     ##  #######  ########  #### ########  #######  ##    ##   \ESC[m\STX##     ##    ###     ######  ##    ## ######## ##       ##     
    \ESC[33m\STX##     ## ##     ## ##     ##  ##       ##  ##     ## ###   ##   \ESC[m\STX##     ##   ## ##   ##    ## ##   ##  ##       ##       ##     
    \ESC[33m\STX##     ## ##     ## ##     ##  ##      ##   ##     ## ####  ##   \ESC[m\STX##     ##  ##   ##  ##       ##  ##   ##       ##       ##     
    \ESC[33m\STX######### ##     ## ########   ##     ##    ##     ## ## ## ##   \ESC[m\STX######### ##     ##  ######  #####    ######   ##       ##     
    \ESC[33m\STX##     ## ##     ## ##   ##    ##    ##     ##     ## ##  ####   \ESC[m\STX##     ## #########       ## ##  ##   ##       ##       ##     
    \ESC[33m\STX##     ## ##     ## ##    ##   ##   ##      ##     ## ##   ###   \ESC[m\STX##     ## ##     ## ##    ## ##   ##  ##       ##       ##     
    \ESC[33m\STX##     ##  #######  ##     ## #### ########  #######  ##    ##   \ESC[m\STX##     ## ##     ##  ######  ##    ## ######## ######## ########
  '';

  notice = ''
    \ESC[1mNOTICE: This shell supports the dhall spec at version 0.10.0 located at https://store.horizon-haskell.net/horizon-spec-0.10.0/\ESC[0m
  '';

  advice = ''
    The following commands are available:

      reformat
      tryToUpgradeEverything

  '';

  module-imports = ''
    import qualified Data.Aeson          as A
    import qualified Data.Aeson.Lens as L
    import qualified Data.Aeson.KeyMap   as A
    import qualified Control.Lens        as L
    import qualified Data.ByteString     as BS
    import qualified Data.List
    import           Data.Kind (Type)
    import           Data.Text (Text)
    import qualified Data.Text           as T
    import qualified Data.Text.Encoding  as T
    import qualified Dhall
    import qualified Dhall.Core
    import qualified Dhall.Pretty        as Dhall
    import qualified Data.Map as Map
    import qualified Horizon.Spec.Lens   as H
    import qualified Horizon.Spec.Pretty as H
    import qualified Horizon.Spec.V0_10  as H
    import qualified Network.HTTP.Simple as HTTP
    import qualified Procex.Prelude      as P ()
    import           Procex.Shell        (cd, initInteractive)
    import qualified Procex.Shell        as P ()
    import           System.Directory    (listDirectory, setCurrentDirectory)
    import           System.Environment  (getEnv, setEnv)
    import qualified Text.Megaparsec as M
    import qualified Text.Megaparsec.Char as M
    import qualified Text.Megaparsec.Char.Lexer as M
  '';

  ghciOptions = [
    "-XDataKinds"
    "-XExtendedDefaultRules"
    "-XGHC2021"
    "-XLambdaCase"
    "-XOverloadedStrings"
    "-XOverloadedLabels"
    "-Wall"
    "-Wno-type-defaults"
  ];

  promptFunction = ''
    :{
    promptFunction :: [String] -> Int -> IO String
    promptFunction _modules _line = do
      d <- getEnv "PWD"
      setCurrentDirectory d
      pure $ "\ESC[33m\STXHorizon: \ESC[m\STX"
     :}
  '';

  ghci-script = ''
    :{
    
      type PVPVersion :: Type
      newtype PVPVersion = MkPVPVersion { fromVersion :: [Int] }
        deriving (Show, Eq, Ord)
    
    
    
      numbers :: M.Parsec Text Text [Int]
      numbers = M.sepBy M.decimal (M.char '.')
    
      hackagePkg :: H.Name -> IO A.Value
      hackagePkg (H.MkName x) = do
        k <- HTTP.parseRequest $ "http://hackage.haskell.org/package/" <> T.unpack x
        HTTP.getResponseBody <$> HTTP.httpJSON k
    
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

      loadHorizon :: IO H.HorizonExport
      loadHorizon = Dhall.inputFile @H.HorizonExport Dhall.auto "horizon.dhall"

      saveHorizon :: H.HorizonExport -> IO ()
      saveHorizon = BS.writeFile "horizon.dhall" . T.encodeUtf8 . Dhall.Core.pretty . H.horizonExportToExpr

      bumpHackage :: H.Name -> IO ()
      bumpHackage x = do
        hz <- loadHorizon
        t <- hackagePkgLatest x
        let f = L.ix x . sourceL . _FromHackage . L.lens H.version (\z y -> z { H.version = y }) L..~ t
        saveHorizon (f hz)

      addHackage :: H.Name -> IO ()
      addHackage x = do
        hz <- loadHorizon
        t <- hackagePkgLatest x
        let f = L.at x L..~ Just (H.callHackage x t)
        saveHorizon (f hz)

      tryToUpgradeEverything :: IO ()
      tryToUpgradeEverything = do
        hz <- loadHorizon
        L.traverseOf_ _Hackages (bumpHackage . H.name) hz

      reformat :: IO ()
      reformat = loadHorizon >>= saveHorizon
    :}
  '';

in
mkCrazyShell {
  inherit name pkgs promptFunction ghciOptions haskellPackages module-imports libraries base-libraries header notice advice ghci-script;
}
