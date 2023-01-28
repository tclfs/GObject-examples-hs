module Main where

import Data.GI.Base
import Data.GI.Base.GType
import Text.Printf

main :: IO ()
main = do
  -- Data.GI.Base.GType does not export these types commented out below, 
  -- except `gtypeInvalid`, which does not have type name
  let types = [ --gtypeInvalid
              -- , gtypeNone
              -- , gtypeInterface
              -- , gtypeChar
              -- , gtypeUChar
              {-,-} gtypeBoolean
              , gtypeInt
              , gtypeUInt
              , gtypeLong
              , gtypeULong
              , gtypeInt64
              , gtypeUInt64
              -- , gtypeEnum
              -- , gtypeFlags
              , gtypeFloat
              , gtypeDouble
              , gtypeString
              , gtypePointer
              , gtypeBoxed
              -- , gtypeParam
              , gtypeObject
              , gtypeGType
              , gtypeVariant
              -- , gtypeChecksum
              ]
      typeS = [-- "G_TYPE_INVALID"
              -- , "G_TYPE_NONE"
              -- , "G_TYPE_INTERFACE"
              -- , "G_TYPE_CHAR"
              -- , "G_TYPE_UCHAR"
              {-,-} "G_TYPE_BOOLEAN"
              , "G_TYPE_INT"
              , "G_TYPE_UINT"
              , "G_TYPE_LONG"
              , "G_TYPE_ULONG"
              , "G_TYPE_INT64"
              , "G_TYPE_UINT64"
              -- , "G_TYPE_ENUM"
              -- , "G_TYPE_FLAGS"
              , "G_TYPE_FLOAT"
              , "G_TYPE_DOUBLE"
              , "G_TYPE_STRING"
              , "G_TYPE_POINTER"
              , "G_TYPE_BOXED"
              -- , "G_TYPE_PARAM"
              , "G_TYPE_OBJECT"
              , "G_TYPE_GTYPE"
              , "G_TYPE_VARIANT"
              -- , "G_TYPE_CHECKSUM"
              ]

  mapM_ (\(t,s) -> do
          name <- gtypeName t
          putStrLn $ printf "The name of %s is %s." s name
        )
        (zip types typeS)