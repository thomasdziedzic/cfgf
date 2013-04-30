import Test.HUnit

import Package.Types
import Package.Library

import qualified Data.Map as M
import qualified Data.Text as T

ghc :: String
ghc = "ghc=7.6.3-1"

testArch :: String
testArch = "x86_64"

pkgTransformers :: PkgDesc
pkgTransformers = PkgDesc "haskell-transformers" "transformers" [0,3,0,0] 4 [ghc, "sh"]

pkgMtl :: PkgDesc
pkgMtl = PkgDesc "haskell-mtl" "mtl" [2,1,2] 3 [ghc, "sh", "haskell-transformers"]

testPkgs :: [PkgDesc]
testPkgs =
    [ pkgTransformers
    , pkgMtl
    ]

getDependencyStringTest_hasDependency :: Test
getDependencyStringTest_hasDependency = TestCase $ assertEqual
    "the dependency string should contain the transformers package"
    " -- -I ../../haskell-transformers/trunk/haskell-transformers-0.3.0.0-4-x86_64.pkg.tar.xz"
    (getDependencyString testPkgs pkgMtl testArch)

getDependencyStringTest_noDependency :: Test
getDependencyStringTest_noDependency = TestCase $ assertEqual
    "the dependency string should not contain any packages"
    ""
    (getDependencyString testPkgs pkgTransformers testArch)

getPackageStringTest :: Test
getPackageStringTest = TestCase $ assertEqual
    "The argument to install the package should match"
    ("-I ../../" ++ pkgname ++ "/trunk/" ++ pkgname ++ "-" ++ pkgver ++ "-" ++ pkgrel ++ "-" ++ testArch ++ ".pkg.tar.xz")
    (getPackageString testArch pkgMtl)
  where
    pkgname = archlinuxName pkgMtl
    pkgver = packageVersionString $ pkgVer pkgMtl
    pkgrel = show $ pkgRel pkgMtl

getPkgVerticesTest_hasDependency :: Test
getPkgVerticesTest_hasDependency = TestCase $ assertEqual
    "The package dependency edges should match"
    [(1, 0)]
    (getPkgVertices (M.fromList [("haskell-transformers", 0), ("haskell-mtl", 1)]) pkgMtl)

getPkgVerticesTest_noDependency :: Test
getPkgVerticesTest_noDependency = TestCase $ assertEqual
    "There should be no package dependency edges"
    []
    (getPkgVertices (M.fromList [("haskell-transformers", 0), ("haskell-mtl", 1)]) pkgTransformers)

bumpTest :: Test
bumpTest = TestCase $ assertEqual
    "transformers should have been bumped the package version and mtl should have bumped the package release"
    [pkgTransformers {pkgVer=[0,4,0,1], pkgRel=1}, pkgMtl {pkgRel=4}]
    (bump testPkgs [[0,4,0,1], [2,1,2]])

bumpPackageTest_repoContainsOldVersion :: Test
bumpPackageTest_repoContainsOldVersion = TestCase $ assertEqual
    "A package with an older should be bumped to the latest version and have a pkgrel set to 1"
    pkgMtl {pkgVer=[2,1,3], pkgRel=1}
    (bumpPackage pkgMtl [2,1,3])

bumpPackageTest_repoContainsSameVersion :: Test
bumpPackageTest_repoContainsSameVersion = TestCase $ assertEqual
    "A package with the same version should have the pkgrel set to pkgrel + 1"
    pkgMtl {pkgRel=4}
    (bumpPackage pkgMtl [2,1,2])

-- TODO find an easy way to test the error condition for bumpPackage, or refactor code so we can test it

packageVersionStringTest :: Test
packageVersionStringTest = TestCase $ assertEqual
    "The package version string should match"
    "2.1.2"
    (packageVersionString $ pkgVer pkgMtl)

contextFromListTest :: Test
contextFromListTest = TestCase $ assertEqual
    "The context should match"
    testValue
    (contextFromList [(testKey, testValue)] testKey)
  where
    testKey = T.pack "key"
    testValue = T.pack "value"

-- TODO find a way to test error exception for contextFromList

fetchVersionedDependsTest_hasDependencies :: Test
fetchVersionedDependsTest_hasDependencies = TestCase $ assertEqual
    "The depends string should match"
    "'haskell-transformers=0.3.0.0-4' 'haskell-mtl=2.1.2-3'"
    (fetchVersionedDepends ["haskell-transformers", "haskell-mtl"] testPkgs)

packageBuildOrderTest :: Test
packageBuildOrderTest = TestCase $ assertEqual
    "The order should be a topological sort of the dependency graph"
    [0, 1]
    (packageBuildOrder testPkgs)

-- TODO write tests for writeTemplate

main :: IO Counts
main = runTestTT $ TestList
    [ getDependencyStringTest_hasDependency
    , getDependencyStringTest_noDependency
    , getPackageStringTest
    , getPkgVerticesTest_hasDependency
    , getPkgVerticesTest_noDependency
    , bumpTest
    , bumpPackageTest_repoContainsOldVersion
    , bumpPackageTest_repoContainsSameVersion
    , packageVersionStringTest
    , contextFromListTest
    , fetchVersionedDependsTest_hasDependencies
    , packageBuildOrderTest
    ]
