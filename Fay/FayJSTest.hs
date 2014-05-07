

{-# LANGUAGE EmptyDataDecls #-}

import Prelude
import FFI

data Func

makeFunc0 :: Int -> Func
makeFunc0 = ffi "function() {return %1;}"

readFunc0 :: Func -> Fay Int
readFunc0 = ffi "%1()"

makeFunc :: Int -> Func
makeFunc = ffi "function(x) {return %1;}"

readFunc :: Func -> Fay Int
readFunc = ffi "%1(1)"

main = do
    let item0 = makeFunc0 6
    readFunc0 item0 >>= print
    let item = makeFunc 6
    readFunc item >>= print
    print "OK"

-- Running this example I would expect the output "6 6 OK", but it's "function() 6 OK".
