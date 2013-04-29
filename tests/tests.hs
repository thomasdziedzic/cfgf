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

main :: IO Counts
main = runTestTT $ TestList
    [ getDependencyStringTest0
    , getDependencyStringTest1
    ]
