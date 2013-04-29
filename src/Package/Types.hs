module Package.Types
    ( ArchlinuxName
    , HackageName
    , PkgVer
    , PkgRel
    , Depends
    , PkgDesc (..)
    ) where

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
    } deriving (Show,Read,Eq)

