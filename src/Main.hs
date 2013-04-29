module Main
  ( main
  )
where

import Package.Types
import Package.Library

import System.Exit
import System.Cmd
import qualified Distribution.Hackage.DB as H
import qualified Data.Map as M
import qualified System.Directory as D
import qualified Distribution.PackageDescription as PD
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO
import qualified Data.Text.Template as TL
import qualified Data.Text.Lazy.IO as LIO
import System.Process (readProcessWithExitCode)

import Paths_cfgf (getDataFileName)

-- leave out cabal-install for now since it is a statically linked binary which doesn't depend on haskell libs or a specific ghc version, also doesn't need an install file
-- PkgDesc "cabal-install" "cabal-install" [1,16,0,2] 2
pkgs :: [PkgDesc]
pkgs = [ PkgDesc "haskell-http" "HTTP" [4000,2,8] 1 [ghc, "sh", "haskell-network", "haskell-parsec", "haskell-mtl"]
       , PkgDesc "haskell-mtl" "mtl" [2,1,2] 3 [ghc, "sh", "haskell-transformers"]
       , PkgDesc "haskell-network" "network" [2,4,1,2] 1 [ghc, "sh", "haskell-parsec"]
       , PkgDesc "haskell-parsec" "parsec" [3,1,3] 3 [ghc, "sh", "haskell-mtl", "haskell-text"]
       , PkgDesc "haskell-random" "random" [1,0,1,1] 5 [ghc, "sh"]
       , PkgDesc "haskell-text" "text" [0,11,2,3] 3 [ghc, "sh"]
       , PkgDesc "haskell-transformers" "transformers" [0,3,0,0] 4 [ghc, "sh"]
       , PkgDesc "haskell-zlib" "zlib" [0,5,4,1] 1 [ghc, "sh", "zlib"]
       ]
  where
    ghc = "ghc=7.6.3-1"

repo :: String
repo = "staging"

archs :: [String]
archs = ["i686", "x86_64"]

main :: IO ()
main = do
    -- update the hackage database
    cabalExitCode <- rawSystem "cabal" ["update"]
    if cabalExitCode == ExitSuccess
        then putStrLn "update succeeded"
        else putStrLn "update failed" >> exitFailure

    -- find the latest version of each package
    hackage <- H.readHackage
    let getHackageMapping (PkgDesc _ hkgName _ _ _) = hackage M.! hkgName
    let hackageMappings = map getHackageMapping pkgs
    let latestVersions = map (H.versionBranch . fst . M.findMax) hackageMappings
    let latestPackageDescriptions = map (PD.packageDescription . snd . M.findMax) hackageMappings

    -- if there is a new version, bump version and reset pkgrel otherwise keep version and bump pkgrel
    let latestPkgs = bump pkgs latestVersions

    -- pkgDepends is the order in which we should build our packages
    let pkgDepends = packageBuildOrder pkgs

    let inorderPkgDescs = map (latestPkgs !!) pkgDepends
    let inorderHkgDescs = map (latestPackageDescriptions !!) pkgDepends

    D.createDirectory "./tmp"
    D.setCurrentDirectory "./tmp"

    mapM_ (buildPkg latestPkgs) $ zip inorderPkgDescs inorderHkgDescs

    D.setCurrentDirectory ".."

buildPkg :: [PkgDesc] -> (PkgDesc, PD.PackageDescription) -> IO ()
buildPkg latestPkgs desc@(pkgDesc, _) = do
    let archName = archlinuxName pkgDesc

    archcoExitCode <- rawSystem "archco" [archName]
    if archcoExitCode == ExitSuccess
        then putStrLn "checkout successful"
        else putStrLn "checkout failed" >> exitFailure

    D.setCurrentDirectory (archName ++ "/trunk")

    generateInstall pkgDesc

    generatePkgbuild desc latestPkgs

    putStrLn "building in chroots"

    buildChroots pkgDesc latestPkgs

    D.setCurrentDirectory "../.."

generateInstall :: PkgDesc -> IO ()
generateInstall pkgDesc = do
    installTemplatePath <- getDataFileName "templates/install.template"
    installContent <- TIO.readFile installTemplatePath

    let installTemplate = TL.template installContent
    let filledTemplate = TL.render installTemplate ctx

    LIO.writeFile filename filledTemplate
  where
    pkgname = archlinuxName pkgDesc
    filename = "./" ++ pkgname ++ ".install"
    ctx = contextFromList [(T.pack "pkgname", T.pack pkgname )]

generatePkgbuild :: (PkgDesc, PD.PackageDescription) -> [PkgDesc] -> IO ()
generatePkgbuild (pkgDesc, hkgPkgDesc) latestPkgs = do
    --pkgbuildTemplatePath <- getDataFileName "templates/PKGBUILD.template"
    pkgbuildContent <- TIO.readFile <=< getDataFileName "templates/PKGBUILD.template"

    let pkgbuildTemplate = TL.template pkgbuildContent
    let filledTemplate = TL.render pkgbuildTemplate (ctx "")

    LIO.writeFile "./PKGBUILD" filledTemplate

    (_, md5sums, _) <- readProcessWithExitCode "makepkg" ["-g"] ""

    let filledTemplatePart2 = TL.render pkgbuildTemplate (ctx md5sums)

    LIO.writeFile "./PKGBUILD" filledTemplatePart2
  where
    pkgname = archlinuxName pkgDesc
    hkgname = hackageName pkgDesc
    pkgver = packageVersionString $ pkgVer pkgDesc
    pkgrel = show . pkgRel $ pkgDesc
    pkgdesc = H.synopsis hkgPkgDesc
    pkgdepends = fetchVersionedDepends (depends pkgDesc) latestPkgs

    ctx md5sums = contextFromList [
          (T.pack "pkgname", T.pack pkgname)
        , (T.pack "hkgname", T.pack hkgname)
        , (T.pack "pkgver", T.pack pkgver)
        , (T.pack "pkgrel", T.pack pkgrel)
        , (T.pack "pkgdesc", T.pack pkgdesc)
        , (T.pack "depends", T.pack pkgdepends)
        , (T.pack "md5sums", T.pack md5sums)
        ]

--generatePkgbuild2 :: (PkgDesc, PD.PackageDescription) -> [PkgDesc] -> L.Text
--generatePkgbuild2 (pkgDesc, hkgPkgDesc) latestPkgs =

buildChroots :: PkgDesc -> [PkgDesc] -> IO ()
buildChroots pkgDesc latestPkgs = do
    mapM_ buildChroots' archs
  where
    buildChroots' arch = do
        putStrLn $ "sudo " ++ repo ++ "-" ++ arch ++ "-build" ++ (getDependencyString latestPkgs pkgDesc arch)
        exitCode <- system $ "sudo " ++ repo ++ "-" ++ arch ++ "-build" ++ (getDependencyString latestPkgs pkgDesc arch)
        case exitCode of
            ExitSuccess -> return ()
            (ExitFailure _) -> exitFailure
