import Test.HUnit

import Package.Types
import Package.Library

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

getDependencyStringTest0 :: Test
getDependencyStringTest0 = TestCase $ assertEqual
    "the dependency string should contain the transformers package"
    " -- -I ../../haskell-transformers/trunk/haskell-transformers-0.3.0.0-4-x86_64.pkg.tar.xz"
    (getDependencyString testPkgs pkgMtl testArch)

getDependencyStringTest1 :: Test
getDependencyStringTest1 = TestCase $ assertEqual
    "the dependency string should not contain any packages"
    ""
    (getDependencyString testPkgs pkgTransformers testArch)

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

-- TODO find an easy way to test the error condition, or refactor code so we can test it

main :: IO Counts
main = runTestTT $ TestList
    [ getDependencyStringTest0
    , getDependencyStringTest1
    , bumpPackageTest_repoContainsOldVersion
    , bumpPackageTest_repoContainsSameVersion
    ]
