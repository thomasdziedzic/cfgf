module Main
  ( PkgDesc(..)
  , main
  )
where

import System.Exit
import System.Cmd
import qualified Distribution.Hackage.DB as H
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import qualified System.Directory as D
import qualified Distribution.PackageDescription as PD
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Template as TL
import qualified Data.Text.Lazy as L
import Data.List (intercalate)
import System.Process (readProcessWithExitCode)
import Data.Maybe

type ArchlinuxName = String
type HackageName = String
type PkgVer = [Int]
type PkgRel = Int
type Depends = [String]

data PkgDesc = PkgDesc
    { archlinuxName :: ArchlinuxName
    , hackageName :: HackageName
    , pkgVer :: PkgVer
    , pkgRel :: PkgRel
    , depends :: Depends
    } deriving (Show,Read)

-- leave out cabal-install for now since it is a statically linked binary which doesn't depend on haskell libs or a specific ghc version, also doesn't need an install file
-- PkgDesc "cabal-install" "cabal-install" [1,16,0,2] 2
pkgs :: [PkgDesc]
pkgs = [ PkgDesc "haskell-http" "HTTP" [4000,2,7] 1 [ghc, "sh", "haskell-network", "haskell-parsec", "haskell-mtl"]
       , PkgDesc "haskell-mtl" "mtl" [2,1,2] 2 [ghc, "sh", "haskell-transformers"]
       , PkgDesc "haskell-network" "network" [2,4,1,0] 1 [ghc, "sh", "haskell-parsec"]
       , PkgDesc "haskell-parsec" "parsec" [3,1,3] 2 [ghc, "sh", "haskell-mtl", "haskell-text"]
       , PkgDesc "haskell-random" "random" [1,0,1,1] 4 [ghc, "sh"]
       , PkgDesc "haskell-text" "text" [0,11,2,3] 2 [ghc, "sh"]
       , PkgDesc "haskell-transformers" "transformers" [0,3,0,0] 3 [ghc, "sh"]
       , PkgDesc "haskell-zlib" "zlib" [0,5,4,0] 1 [ghc, "sh", "zlib"]
       ]
  where
    ghc = "ghc=7.6.2-1"

repo :: String
repo = "testing"

chroots :: String
chroots = "/var/lib/archbuild"

bump :: [PkgDesc] -> [PkgVer] -> [PkgDesc]
bump [] [] = []
bump (p:ps) (v:vs) =
    case compare (pkgVer p) v of
        LT -> p {pkgVer=v, pkgRel=1} : bump ps vs
        EQ -> p {pkgRel=((pkgRel p) + 1)} : bump ps vs
        GT -> error $ "the latest version is less than the current package version, old version: " ++ show (pkgVer p) ++ " new version: " ++ show v
bump _ _ = error "the lists don't have the same length"

getPkgVertices :: M.Map String Int -> PkgDesc -> [(Int,Int)]
getPkgVertices vertexMap pkgDesc = zip (repeat currentPkgVertex) dependVertices
  where
    currentPkgVertex = vertexMap M.! (archlinuxName pkgDesc)
    dependVertices = buildDependVertices $ depends pkgDesc
    buildDependVertices [] = []
    buildDependVertices (x:xs) = case M.lookup x vertexMap of
        Nothing -> buildDependVertices xs
        Just a -> a : buildDependVertices xs

main :: IO ()
main = do
    -- update the hackage database
    cabalExitCode <- rawSystem "cabal" ["update"]
    if cabalExitCode == ExitSuccess
        then putStrLn "update succeeded"
        else putStrLn "update failed" >> exitFailure

    -- find the latest version of each package
    hackage <- H.readHackage
    let getHackageMapping (PkgDesc archlinuxName hackageName oldPkgVer oldPkgDesc depends) = hackage M.! hackageName
    let hackageMappings = map getHackageMapping pkgs
    let latestVersions = map (H.versionBranch . fst . M.findMax) hackageMappings
    let latestPackageDescriptions = map (PD.packageDescription . snd . M.findMax) hackageMappings

    -- if there is a new version, bump version and reset pkgrel otherwise keep version and bump pkgrel
    let latestPkgs = bump pkgs latestVersions

    -- create haskell package dependency graph
    let bounds = (0, length pkgs - 1)
    let pkgNames = map archlinuxName pkgs
    let pkgNameToVertex = M.fromList $ zip pkgNames [0..]
    let pkgEdges = concatMap (getPkgVertices pkgNameToVertex) pkgs
    -- pkgDepends is the order in which we should build our packages
    let pkgDepends = G.topSort $ G.transposeG $ G.buildG bounds pkgEdges
    let pkgNames = map (archlinuxName . (pkgs !!)) pkgDepends
    let inorderPkgDescs = map (latestPkgs !!) pkgDepends

    -- build dependency graph in post order traversal
    let archlinuxNameToPkgDesc = M.fromList $ map (\x -> (archlinuxName x, x)) latestPkgs

    D.createDirectory "./tmp"
    D.setCurrentDirectory "./tmp"

    updateChroots

    mapM_ (buildPkg latestPkgs) $ zip inorderPkgDescs latestPackageDescriptions

    D.setCurrentDirectory ".."

-- TODO implement this
buildPkg :: [PkgDesc] -> (PkgDesc, PD.PackageDescription) -> IO ()
buildPkg latestPkgs desc@(pkgDesc, hkgPkgDesc) = do
    let archName = archlinuxName pkgDesc
    let deps = depends pkgDesc

    archcoExitCode <- rawSystem "archco" [archName]
    if archcoExitCode == ExitSuccess
        then putStrLn "checkout successful"
        else putStrLn "checkout failed" >> exitFailure

    D.setCurrentDirectory (archName ++ "/trunk")

    generateInstall desc

    generatePkgbuild desc latestPkgs

    setupChroots latestPkgs desc

    buildChroots desc

    D.setCurrentDirectory "../.."

generateInstall :: (PkgDesc, PD.PackageDescription) -> IO ()
generateInstall (pkgDesc, hkgPkgDesc) = do
    installContent <- TIO.readFile "../../../templates/install.template"

    let installTemplate = TL.template installContent
    let filledTemplate = TL.render installTemplate ctx

    let getOffYourButt = L.toStrict filledTemplate
    TIO.writeFile filename getOffYourButt
  where
    pkgname = archlinuxName pkgDesc
    filename = "./" ++ pkgname ++ ".install"
    ctx = contextFromList [(T.pack "pkgname", T.pack pkgname )]

generatePkgbuild :: (PkgDesc, PD.PackageDescription) -> [PkgDesc] -> IO ()
generatePkgbuild (pkgDesc, hkgPkgDesc) latestPkgs = do
    pkgbuildContent <- TIO.readFile "../../../templates/PKGBUILD.template"

    let pkgbuildTemplate = TL.template pkgbuildContent
    let filledTemplate = TL.render pkgbuildTemplate (ctx "")

    let getOffYourButt = L.toStrict filledTemplate
    TIO.writeFile "./PKGBUILD" getOffYourButt

    (_, md5sums, _) <- readProcessWithExitCode "makepkg" ["-g"] ""

    let filledTemplate = TL.render pkgbuildTemplate (ctx md5sums)

    let getOffYourButt = L.toStrict filledTemplate
    TIO.writeFile "./PKGBUILD" getOffYourButt
  where
    pkgname = archlinuxName pkgDesc
    hkgname = hackageName pkgDesc
    pkgver = intercalate "." $ map show $ pkgVer pkgDesc
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

fetchVersionedDepends :: [String] -> [PkgDesc] -> String
fetchVersionedDepends deps latestPkgs
    | null pkgdeps = ""
    | otherwise = "'" ++ pkgdeps ++ "'"
  where
    archlinuxNames = S.fromList $ map archlinuxName latestPkgs
    pkgnameToPkgver = M.fromList [(archlinuxName p, archlinuxName p ++ "=" ++ (intercalate "." $ map show $ pkgVer p) ++ "-" ++ (show $ pkgRel p)) | p <- latestPkgs, (archlinuxName p) `S.member` archlinuxNames]
    pkgdeps = intercalate "' '" $ map (\x -> maybe x id (M.lookup x pkgnameToPkgver)) deps

updateChroots :: IO ()
updateChroots = do
    putStrLn "updating chroots"

    let arch = "i686"
    exitCode <- rawSystem "sudo" [
          "setarch", arch
        , "mkarchroot", "-u"
        , "-C", "/usr/share/devtools/pacman-" ++ repo ++ ".conf"
        , "-M", "/usr/share/devtools/makepkg-" ++ arch ++ ".conf"
        , chroots ++ "/" ++ repo ++ "-" ++ arch ++ "/root"
        ]
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> exitFailure

    let arch = "x86_64"
    exitCode <- rawSystem "sudo" [
          "setarch", arch
        , "mkarchroot", "-u"
        , "-C", "/usr/share/devtools/pacman-" ++ repo ++ ".conf"
        , "-M", "/usr/share/devtools/makepkg-" ++ arch ++ ".conf"
        , chroots ++ "/" ++ repo ++ "-" ++ arch ++ "/root"
        ]
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> exitFailure

buildChroots :: (PkgDesc, PD.PackageDescription) -> IO ()
buildChroots (pkgDesc, hkgPkgDesc) = do
    putStrLn "building in chroots"

    let arch = "i686"
    exitCode <- system $ "sudo setarch " ++ arch ++ " makechrootpkg " ++ cleanFlag ++ " -r " ++ chroots ++ "/" ++ repo ++ "-" ++ arch
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> exitFailure

    let arch = "x86_64"
    exitCode <- system $ "sudo setarch " ++ arch ++ " makechrootpkg " ++ cleanFlag ++ " -r " ++ chroots ++ "/" ++ repo ++ "-" ++ arch
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> exitFailure
  where
    cleanFlag = if (length . depends) pkgDesc == 0 then "-c" else ""

setupChroots :: [PkgDesc] -> (PkgDesc, PD.PackageDescription) -> IO ()
setupChroots latestPkgs (pkgDesc, hkgPkgDesc) = do
    cwd <- D.getCurrentDirectory

    D.setCurrentDirectory "../.."

    case inorderPkgDescs of
        [] -> return ()
        [x] -> installPkg x True
        x:xs -> do
            installPkg x True
            mapM_ (flip installPkg False) xs

    D.setCurrentDirectory cwd
  where
    pkgnameToPkgDesc = M.fromList [(archlinuxName p, p) | p <- latestPkgs]

    bounds = (0, length pkgs - 1)
    pkgNames = map archlinuxName pkgs
    pkgNameToVertex = M.fromList $ zip pkgNames [0..]
    pkgEdges = concatMap (getPkgVertices pkgNameToVertex) pkgs
    pkgDepends = G.topSort $ G.transposeG $ G.buildG bounds pkgEdges

    pkgNames2 = map (archlinuxName . (pkgs !!)) pkgDepends

    depsInOrder = filter (flip S.member depsSet) pkgNames2

    archlinuxNameToPkgDesc = M.fromList $ map (\x -> (archlinuxName x, x)) latestPkgs

    inorderPkgDescs = map (archlinuxNameToPkgDesc M.!) depsInOrder

    -- retrieve deps recursively
    deps = S.toList . S.fromList $ deps' $ depends pkgDesc
    deps' :: [String] -> [String]
    deps' [] = []
    deps' xs = validPkgNames ++ (deps' dependsOfXs)
       where
           validPkgNames :: [String]
           validPkgNames = filter (flip M.member pkgnameToPkgDesc) xs
           xsPkgDescs = mapMaybe (flip M.lookup pkgnameToPkgDesc) xs
           dependsOfXs :: [String]
           dependsOfXs = concatMap depends xsPkgDescs
    --rebuiltDeps = mapMaybe (flip M.lookup pkgnameToPkgDesc) deps
    depsSet = S.fromList deps

installPkg :: PkgDesc -> Bool -> IO ()
installPkg latestPkg clean= do
    let arch = "i686"
    exitCode <- system $ "sudo setarch " ++ arch ++ " makechrootpkg " ++ cleanFlag ++ " -r " ++ chroots ++ "/" ++ repo ++ "-" ++ arch ++ " -I " ++ pkgname ++ "/trunk/" ++ pkgname ++ "-" ++ pkgver ++ "-" ++ pkgrel ++ "-" ++ arch ++ ".pkg.tar.xz"
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> exitFailure

    let arch = "x86_64"
    exitCode <- system $ "sudo setarch " ++ arch ++ " makechrootpkg " ++ cleanFlag ++ " -r " ++ chroots ++ "/" ++ repo ++ "-" ++ arch ++ " -I " ++ pkgname ++ "/trunk/" ++ pkgname ++ "-" ++ pkgver ++ "-" ++ pkgrel ++ "-" ++ arch ++ ".pkg.tar.xz"
    case exitCode of
        ExitSuccess -> return ()
        (ExitFailure code) -> exitFailure
  where
    pkgname = archlinuxName latestPkg
    pkgver = intercalate "." $ map show $ pkgVer latestPkg
    pkgrel = show . pkgRel $ latestPkg
    cleanFlag = if clean then "-c" else ""

contextFromList :: [(T.Text, T.Text)] -> TL.Context
contextFromList assocs x = maybe err id . lookup x $ assocs
  where
    err = error $ "Could not find key: " ++ T.unpack x
