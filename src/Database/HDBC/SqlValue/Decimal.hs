{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.HDBC.SqlValue.Decimal () where

import Data.Convertible (Convertible (safeConvert), ConvertResult, convError)
import Data.Decimal (Decimal)
import Data.Dynamic (Typeable)
import Database.HDBC.SqlValue (SqlValue (SqlString))

instance Convertible Decimal SqlValue where
    safeConvert = return . SqlString . show

instance Convertible SqlValue Decimal where
    safeConvert (SqlString x) = read' x
    safeConvert x = read' =<< safeConvert x

read' :: (Typeable a, Read a, Convertible SqlValue a) => String -> ConvertResult a
read' s = case reads s of
              [(x,"")] -> Right x
              _ -> convError "Cannnot read source value as dest type" (SqlString s)

