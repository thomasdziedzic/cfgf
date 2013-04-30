module Package.Library
    ( getDependencyString
    , getPackageString
    , getPkgVertices
    , bump
    , bumpPackage
    , packageVersionString
    , contextFromList
    , fetchVersionedDepends
    , packageBuildOrder
    , writeTemplate
    ) where

import Package.Types

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Graph as G
import Data.List (intercalate, find)
import Data.Maybe (fromJust, mapMaybe,fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Template as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LIO
import Control.Monad ((<=<))
import Paths_cfgf (getDataFileName)

-- used to get the string we need to append to archbuild to install our dependencies
getDependencyString :: [PkgDesc] -> PkgDesc -> String -> String
getDependencyString latestPkgs pkgDesc arch =
    if null finalString then "" else " -- " ++ finalString
  where
    bounds = (0, length latestPkgs - 1)
    pkgNames = map archlinuxName latestPkgs
    pkgNameToVertex = M.fromList $ zip pkgNames [0..]
    pkgEdges = concatMap (getPkgVertices pkgNameToVertex) latestPkgs
    dependencyGraph = G.buildG bounds pkgEdges
    pkgDepends = G.topSort $ G.transposeG dependencyGraph

    currentPkgIndex = fst . fromJust $ find ((archlinuxName pkgDesc ==) .  archlinuxName . snd) $ zip [0..] latestPkgs
    vertexDepends = S.fromList $ G.reachable dependencyGraph currentPkgIndex
    inorderVertexDepends = filter (`S.member` vertexDepends) pkgDepends
    inorderVertexDepends' = filter (currentPkgIndex /=) inorderVertexDepends
    inorderPkgDescs = map (latestPkgs !!) inorderVertexDepends'
    finalString = unwords $ map (getPackageString arch) inorderPkgDescs

getPackageString :: String -> PkgDesc -> String
getPackageString arch latestPkg =
    "-I ../../" ++ pkgname ++ "/trunk/" ++ pkgname ++ "-" ++ pkgver ++ "-" ++ pkgrel ++ "-" ++ arch ++ ".pkg.tar.xz"
  where
    pkgname = archlinuxName latestPkg
    pkgver = packageVersionString $ pkgVer latestPkg
    pkgrel = show . pkgRel $ latestPkg

getPkgVertices :: M.Map String Int -> PkgDesc -> [(Int,Int)]
getPkgVertices vertexMap pkgDesc = zip (repeat currentPkgVertex) dependVertices
  where
    currentPkgVertex = vertexMap M.! archlinuxName pkgDesc
    dependVertices = buildDependVertices $ depends pkgDesc
    buildDependVertices dependencies = mapMaybe (flip M.lookup vertexMap) dependencies

bump :: [PkgDesc] -> [PkgVer] -> [PkgDesc]
bump = zipWith bumpPackage

bumpPackage :: PkgDesc -> PkgVer -> PkgDesc
bumpPackage package hackageVersion =
    case compare (pkgVer package) hackageVersion of
        LT -> package {pkgVer=hackageVersion, pkgRel=1}
        EQ -> package {pkgRel=bumpedPackageRelease}
        GT -> error $ packageName ++ "  has a newer version in the arch repos, arch repo version: " ++ repoVersionString ++ " hackage version: " ++ hackageVersionString
  where
    packageName = archlinuxName package
    bumpedPackageRelease = pkgRel package + 1
    repoVersionString = show $ pkgVer package
    hackageVersionString = show hackageVersion

packageVersionString :: PkgVer -> String
packageVersionString packageVersion = intercalate "." $ map show packageVersion

contextFromList :: [(T.Text, T.Text)] -> TL.Context
contextFromList assocs x = fromMaybe err . lookup x $ assocs
  where
    err = error $ "Could not find key: " ++ T.unpack x

fetchVersionedDepends :: [String] -> [PkgDesc] -> String
fetchVersionedDepends deps latestPkgs
    | null pkgdeps = ""
    | otherwise = "'" ++ pkgdeps ++ "'"
  where
    archlinuxNames = S.fromList $ map archlinuxName latestPkgs
    pkgnameToPkgver = M.fromList [(archlinuxName p, archlinuxName p ++ "=" ++ (packageVersionString $ pkgVer p) ++ "-" ++ show (pkgRel p)) | p <- latestPkgs, archlinuxName p `S.member` archlinuxNames]
    pkgdeps = intercalate "' '" $ map (\x -> fromMaybe x (M.lookup x pkgnameToPkgver)) deps

packageBuildOrder :: [PkgDesc] -> [Int]
packageBuildOrder packages = G.topSort $ G.transposeG $ G.buildG bounds pkgEdges
  where
    bounds = (0, length packages - 1)
    pkgNames = map archlinuxName packages
    pkgNameToVertex = M.fromList $ zip pkgNames [0..]
    pkgEdges = concatMap (getPkgVertices pkgNameToVertex) packages

-- Untested
writeTemplate :: FilePath -> TL.Context -> FilePath -> IO ()
writeTemplate templatePath context targetPath = do
    templateContent <- TIO.readFile <=< getDataFileName $ templatePath

    LIO.writeFile targetPath $ TL.substitute templateContent context
