{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
import Control.Monad
import Data.Maybe
import Data.List (sortBy, isPrefixOf, isSuffixOf)
import Data.Monoid
import Data.Ord

import Stg.Syntax
import Stg.Pretty
import Stg.IO

import System.Environment
import Codec.Archive.Zip

import Control.Monad.IO.Class
import qualified Data.Map as M
import System.FilePath

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Codec.Serialise
import System.Directory
import qualified Data.Text.Encoding as T

import qualified Stg.GHC.Symbols as GHCSymbols
import Stg.Program

import Data.Aeson
import GHC.Generics (Generic)

main :: IO ()
main = do
  [p] <- getArgs
  let outdir = replaceExtension p ".trufflestg"
  doesDirectoryExist outdir >>= \b -> when b $ removeDirectoryRecursive outdir
  createDirectory outdir

  modinfoList <- getAppModuleMapping p
  appModpaks <- collectProgramModules' False (map modModpakPath modinfoList) "main" "Main" GHCSymbols.liveSymbols
  createDirectory (outdir </> "stg")
  forM_ appModpaks $ \nm -> do
    x <- readModpakL nm modpakStgbinPath decodeStgbin'
    let modn = getModuleName $ moduleName x
    liftIO $ writeFileSerialise (outdir </> "stg" </> BS8.unpack modn) x
    print modn
  info <- getAppInfo p
  encodeFile (outdir </> "appinfo.json") info
  createDirectory (outdir </> "libs")
  forM_ (appLdOptions info) $ \ldopt -> when ("-lHS" `isPrefixOf` ldopt) $ do
    let basename = "lib" ++ drop 2 (dropEnd 10 ldopt)
    forM_ (appLibPaths info) $ \libdir -> do
      listDirectory libdir >>= traverse \f -> do
        when (basename `isPrefixOf` f &&
          (".dyn_o_cbits.a" `isSuffixOf` f || ".dyn_o_stubs.a" `isSuffixOf` f)) $ do
            copyFile (libdir </> f) (outdir </> "libs" </> f)

dropEnd n = reverse . drop n . reverse

instance ToJSON StgAppInfo
deriving instance Generic StgAppInfo

deriving newtype instance Serialise DataConId
deriving newtype instance Serialise BinderId
deriving newtype instance Serialise TyConId
deriving newtype instance Serialise UnitId
deriving newtype instance Serialise ModuleName

instance Serialise Unique
instance Serialise PrimElemRep
instance Serialise PrimRep
instance Serialise Type
instance Serialise IdDetails
instance Serialise Scope
instance Serialise Binder
instance Serialise SBinder
instance Serialise LitNumType
instance Serialise LabelSpec
instance Serialise Lit
instance Serialise SourceText
instance Serialise CCallTarget
instance Serialise CCallConv
instance Serialise Safety
instance Serialise ForeignCall
instance Serialise PrimCall
instance Serialise UpdateFlag
instance Serialise StgOp
instance Serialise DataConRep
instance Serialise SDataCon
instance Serialise STyCon
instance Serialise ForeignStubs
instance Serialise ForeignSrcLang
instance Serialise RealSrcSpan
instance Serialise BufSpan
instance Serialise SrcSpan
instance Serialise Tickish
instance (Serialise tcOcc) => Serialise (AltType' tcOcc)
instance (Serialise dcOcc) => Serialise (AltCon' dcOcc)
instance (Serialise idOcc) => Serialise (Arg' idOcc)
instance (Serialise idBnd, Serialise idOcc, Serialise dcOcc, Serialise tcOcc) => Serialise (TopBinding' idBnd idOcc dcOcc tcOcc)
instance (Serialise idBnd, Serialise idOcc, Serialise dcOcc, Serialise tcOcc) => Serialise (Binding' idBnd idOcc dcOcc tcOcc)
instance (Serialise idBnd, Serialise idOcc, Serialise dcOcc, Serialise tcOcc) => Serialise (Rhs' idBnd idOcc dcOcc tcOcc)
instance (Serialise idBnd, Serialise idOcc, Serialise dcOcc, Serialise tcOcc) => Serialise (Alt' idBnd idOcc dcOcc tcOcc)
instance (Serialise idBnd, Serialise idOcc, Serialise dcOcc, Serialise tcOcc) => Serialise (Expr' idBnd idOcc dcOcc tcOcc)
instance (Serialise idBnd, Serialise idOcc, Serialise dcOcc, Serialise tcOcc, Serialise tcBnd) => Serialise (Module' idBnd idOcc dcOcc tcOcc tcBnd)
