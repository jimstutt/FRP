{-# LANGUAGE NoImplicitPrelude, EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}

import Fay.Text (Text, fromString)
import qualified Fay.Text as T
import FFI
import Prelude

data JValue

data Type = Type
    {
        keyName :: T.Text
    }

key :: Automatic Type
key = ffi "{'keyName':'key'}"

hash :: JValue
hash = ffi "{'key':'value'}"

jvValue :: JValue -> T.Text -> JValue
jvValue v k = jvValue' v (T.unpack k)

jvValue' :: JValue -> String -> JValue
jvValue' = ffi "%1[%2]"

jvValue1 :: JValue -> T.Text -> JValue
jvValue1 = ffi "%1[%2]"

jvGetString :: JValue -> T.Text
jvGetString = ffi "%1 + ''"

main = do
    let typeV = key
    let keyV = keyName typeV
    print "test1"
    putStrLn $ T.unpack $ jvGetString $ jvValue hash keyV
    print "test2"
    putStrLn $ T.unpack $ jvGetString $ jvValue1 hash keyV

-- I compile this with: fay --package fay-text --html-wrapper FayTest2.hs.
