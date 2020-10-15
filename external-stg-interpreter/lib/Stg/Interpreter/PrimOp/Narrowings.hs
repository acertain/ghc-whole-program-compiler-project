{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Narrowings where

import Data.Int
import Data.Word

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i  = Literal (LitNumber LitNumInt i)
pattern WordV w = Literal (LitNumber LitNumWord w)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- narrow8Int# :: Int# -> Int#
  ("narrow8Int#", [IntV a])     -> pure [IntV  $ fromIntegral (fromIntegral a :: Int8)]

  -- narrow16Int# :: Int# -> Int#
  ("narrow16Int#", [IntV a])    -> pure [IntV  $ fromIntegral (fromIntegral a :: Int16)]

  -- narrow32Int# :: Int# -> Int#
  ("narrow32Int#",  [IntV a])   -> pure [IntV  $ fromIntegral (fromIntegral a :: Int32)]

  -- narrow8Word# :: Word# -> Word#
  ("narrow8Word#", [WordV a])   -> pure [WordV $ fromIntegral (fromIntegral a :: Word8)]

  -- narrow16Word# :: Word# -> Word#
  ("narrow16Word#", [WordV a])  -> pure [WordV $ fromIntegral (fromIntegral a :: Word16)]

  -- narrow32Word# :: Word# -> Word#
  ("narrow32Word#", [WordV a])  -> pure [WordV $ fromIntegral (fromIntegral a :: Word32)]

  _ -> fallback op args t tc
