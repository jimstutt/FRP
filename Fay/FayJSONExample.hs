{-# LANGUAGE NoImplicitPrelude #-}

module Console where

import           Language.Fay.FFI
import           Language.Fay.Prelude

data MyData = MyData  { xVar :: Int, yVar :: Int }

myData :: MyData
myData = MyData { yVar = 3, xVar = 9 }

main = do
	jsonSerialized <- toJSON myData
	jsonDeserialized <- toMyData jsonSerialized
	printBool $ (xVar myData == xVar jsonDeserialized)
	print "Now Testing Data Generated by Aeson Library"
	aesonData <- toMyData "{\"xVar\":3,\"yVar\":-1}"
	printInt $ yVar aesonData

-- | Print using console.log.
print :: String -> Fay ()
print = ffi "console.log(%1)"

printBool :: Bool -> Fay ()
printBool = ffi "console.log(%1)"

printInt :: Int -> Fay ()
printInt = ffi "console.log(%1)"

toMyData :: String -> Fay MyData
toMyData = ffi "JSON.parse(%1)"

toJSON :: MyData -> Fay String
toJSON = ffi "JSON.stringify(%1)"