{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Prelude
import Data.Data
import qualified Data.Aeson as A
import Data.String.Here.Uninterpolated
import Fay.Convert
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as B8

data E = E
  { int :: Int,
    string :: String }
  deriving (Typeable, Data, Read, Show)

e = E 1 "one"
$(deriveJSON id ''E)

{-
Dear Fay people,

Greetings fellow fayers.  I am trying to figure out how to move
data from the browser (running fay code) to the server (running
Snap code).  I've looked though quite a bit of code, but cannot
really seem to find the magic incantation.  In particular, I've
looked at the example in snaplet-fay, which seems to use some (to
me) magical javascript in index.js called formJson.

I've also looked at Client.hs and Server.hs in the fay-server
package, but don't quite get it.  I can see the stringify going
out in line 19 of Client.API and in line 33 of Server.API the
code: decode json >>= readFromFay statement, which looks like json
coming in and a Haskell data coming out, so I thought I would try
it out on a trivial example to see what is going on.

This leads me to the code below.  I wanted to stringify a simple data
type, E, with fay using JSON.stringify, which I did with the commented
out program below, but then the problem was how do I get my orginal
data E back?  Decoding the string produced by stringify with aeson
decode yields Nothing.  It looks like showToFay and readFromFay are
inverses, but I can't run showToFay in the browser, just stringify?  I
have to admit to being quite confused.  Can any of you shed any light?
Once I get this working, I'll have a very juicy piece of fay code
running that allows people to easily enter calcudoku puzzles
(www.calcudoku.org).  I will write it up with anansi so that others
can benefit from it.

While I'm at it, I have one more fay question.  In the code that
reads users events and produces the haskell representation of the
calcudoku puzzle, I am passing around a "global reference" to 90%
of the functions in the module, which essentially holds the
current state of the "world."  I would use the State monad if it
were available in fay, but it isn't.  Is there some prettier way
to do this?  Any pointers would be appreciated.

Best wishes,
Henry Laxen

-}


main = do
  let a1 = A.decode js :: Maybe E
  print a1  -- Nothing
  let a2 = A.encode e
  print a2  -- "{\"string\":\"one\",\"int\":1}"
  let a3 = A.decode a2 :: Maybe E
  print a3  -- Just (E {int = 1, string = "one"})
  let Just a4 = showToFay e
  print a4  -- Object fromList [("string",String "one"),("instance",String "E"),("int",Number 1)]
  let Just a5 = readFromFay a4 :: Maybe E
  print a5 -- E {int = 1, string = "one"}

-- js is what I get when I use Fay to stringify E
-- it was produced by running the fay compiled program below through nodejs
js :: B8.ByteString
js =  B8.pack [here|{"instance":"E","int":1,"string":"one"}|]

{-

import Prelude
import Data.Data
import JQuery
import FFI

data E = E
  { int :: Int,
    string :: String }
  deriving (Typeable, Data, Read, Show)

e = E 1 "one"

main :: Fay ()
main = do
  let a = showE e
  putStrLn a

showE :: E -> String
showE = ffi "JSON.stringify(%1)"

-- produces:
{"instance":"E","int":1,"string":"one"}

-}

