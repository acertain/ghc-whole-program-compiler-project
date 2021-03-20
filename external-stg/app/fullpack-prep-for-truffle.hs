{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
import Control.Monad
import Data.Maybe
import Data.List (sortBy)
import Data.Monoid
import Data.Ord

import Stg.Syntax
import Stg.Pretty
import Stg.IO

import System.Environment
import Codec.Archive.Zip

import Control.Monad.IO.Class
import qualified Data.Map as M

import Control.Monad
import System.FilePath

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Codec.Serialise
import System.Directory
import qualified Data.Text.Encoding as T


main :: IO ()
main = do
  [p] <- getArgs
  let outdir = replaceExtension p ".truffleghc"
  createDirectoryIfMissing False outdir
  withArchive p $ do
    ents <- getEntries
    forM_ (M.toList ents) $ \(es,ed) -> do
      let es' = unEntrySelector es
      when (takeExtension es' == ".stgbin") $ do
        x <- decodeStgbin' . BSL.fromStrict <$> getEntry es

        liftIO $ writeFileSerialise (outdir </> takeDirectory es') x
        -- writeFile FilePath String
        liftIO $ print es



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
