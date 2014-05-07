import Prelude
import FFI

main = ready $ do
    writeLog "before appending"
    select "<h1>Hello, world</h1>" >>= appendTo "body"
    writeLog "after appending"
