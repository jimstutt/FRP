{-
Nadine and Henry's Calcudoku Solver

Henry Laxen April 24, 2013
Background
First I would like to thank Chris Done and Adam Bergmark for their superb job in creating Fay, a way to write Javascript without writing Javascript. I've always disliked Javascript. I'm not sure why, but it is probably because of my Forth heritage. I remember a saying, I think by Bill Ragsdale: "Inside every big program is a small program trying to get out." That, for me, summarizes how I feel about Javascript. But now, thanks to Chris and Adam, I get to program in the best of all possible programming languages, Haskell. (My apologies to Voltaire.)

A couple of years ago, my friend Bonnie asked me to help her solve a Calcudoku puzzle. Rather than sit down and try to figure it out, I decided that since I was a powerful, master of the universe, Haskell programmer, I would just write a solver and short circuit all future questions. So I fired up emacs and went to the Calcudoku/ site, imported TagSoup, and started parsing. Next I wrote a solver, using Control.Monad.Logic, and voila, problem solved. However I didn't like that my solver was intimately tied to one website. I wanted a way to conveniently input a Calcudoku puzzle and get back a result. This meant I would have to write Javascript, and a lot of it. Sorry, no can do. Well, once Fay came along, I thought, why not finally write my inputter. While I was at it, I might as well document it so that it might still mean something to me in a couple of months, and perhaps help you, gentle reader, if you struggle with some the same issues I did.
Design
At this point you may want to take a peek at http://www.nadineloveshenry.com/calcudoku/index and scroll down to where you see the heading Enter the data yourself. There you will see a lonely select list, asking for the Puzzle Size. Once you select a puzzle size, a bunch of other widgets appear, waiting for your input. Obviously there is a lot of ajax going on behind the scenes.

In order to solve a Calcudoku puzzle, we need to know a few things. First the puzzle size. Next the range of values to be used for the puzzle data. Next the type of puzzle, there are three defined at the Calcudoku site, namely single puzzles, double puzzles, and so called killer puzzles. These are global pieces of data that describe the puzzle in general. Next each puzzle consists of regions, and each region has associated with it a value, an operator, and a set of cells. This code is supposed to make the input of this data the least tedious it can be. Once you enter the operator (from a select list) and a value in the text box, you should proceed to click on the cells that comprise the region. While entering the region, you can change the value and the operator, but once the region is finished, these cannot be changed. You can press [Enter] to complete the region, or click on the finish region button. Clicking on a cell while defining a region toggles its membership in the region, so if you accidently added a cell you didn't mean to add, just click on it again and you'll remove it from the region. Once you've finished a region, if you discover an error you can always remove regions in the reverse order in which they were defined by clicking on the discard last region button. Once the last region is finished, the data is sent to the server and the response is received and displayed. That's all there is to it!

Now that we know what we want to do, let's do it. As usual, I'll start at the bottom by first describing the types that we will use.
Shared Types
First a few words about why Fay is really amazing. It's all in the types, dude. The beauty of using Fay is that you can define your Types, use them in your Fay/Javascript client code, and then turn right around and use them in your Snap/Haskell server code. No converting to/from json or sql or strings or whatever. It is all done for you and you don't really have to care. This means you get that angel on your shoulder, the Haskell typechecker, for free. This alone is reason enough to use Fay, but for me the most compelling reason was still, it isn't javascript.

For starters, lets get the declarations and imports out of the way.
-}
-- «sharedTypes imports»
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Calcudoku.SharedTypes where

import Prelude (Maybe, Eq, Read, Show, Int)
import Data.Data
{-
I define a couple of type synonyms to make things clearer, and then define the Coord type, which is just the row and column of a cell in the puzzle. The first time I wrote this, I just used a tuple for this, but Fay gave me a lot of trouble with that, (when I tried to encode and decode the data, and also with pattern matching) so I decided it was easier to just define a new data type and let emacs query-replace go to work for me.
-}
-- «sharedTypes Coord»
type Row = Int
type Column = Int
type Value = Int

data Coord              = C {
  cRow          :: Row,
  cColumn       :: Column }
    deriving (Eq, Typeable, Data, Read, Show)
{-
Next are the data types for the operations and the puzzle type. The non commutative operations work as follows. For Minus one of the cells holds the first value and the rest are subtracted from it. Similar for Divide, one if the cells in the Divide region is the dividend, and the rest are the divisors. A Mod region can have two cells, and Id can only be a one cell region. I use Empty internally for a basically non-existant region.

The Subscriber PuzzleType constructor was an afterthought, and is not used in the Fay Client portion of the code. It us used in case the user tries to parse a puzzle from the calcudoku.org website that is for subscibers only.
-}
«sharedTypes Region»
data Operation          = Plus   | Minus  | Times  | Divide | Power
                        | Or     | And    |  Mod   | Id     | Empty
  deriving (Eq, Typeable, Data, Read, Show)

data PuzzleType         = Single | Double | Killer | Subscriber | NullP
  deriving (Eq, Typeable, Data, Read, Show)

data Region             = R {
  regionResult           :: Value,
  regionOperation        ::  Operation,
  regionCoords           :: [Coord] }
  deriving (Eq, Typeable, Data, Read, Show)
{-
Finally Constraints are just lists of Regions, and a Board is a collection of data that totally defines the puzzle. Board is the important Data type that is shared between the client and the server.
-}
«sharedTypes Board»
data Constraints        = Constraints 
  { regions             :: [Region] }
  deriving (Eq, Typeable, Data, Read, Show)
           
data Board              = Board {
  puzzleType            :: PuzzleType,
  puzzleSide            :: Int,
  puzzleElementRange    :: [Int],
  puzzleConstraints     :: [Constraints] }
  -- puzzleConstraints is a list because when we solve a Double PuzzleType
  -- we use flipConstraints to switch the constraints from one board
  -- to another.  Thus head puzzleConstraints is the current list of Regions
  deriving (Eq, Typeable, Data, Read, Show)
{-
Client Types
There is one more type I need for the client, but I don't need it for the server. One of the nice things anansi does for me is it allows me to define things in a logical order and place, and then move them around into seperate files like Haskell likes. So the following data type actually appears in a file called ClientTypes.hs, even though I define it here along with all of the shared types. Because the Fay client code needs a notion of the current region, I added a simple data type, used throughout the client, that includes a Board and the current region.

After running this for a few days, I started getting emails that the puzzle didn't have a solution. I needed a way for the user to be able to send me the data that he input, so I could take a look at it. That required a new datatype, the CEvent. Each CEvent corresponds to a user's action. By replaying the user input, I can debug anything that goes wrong in the Fay code, as well as make sure the solver is running correctly. So far, all of the errors have been mistakes on the data entry part. This means I need to rethink how to make the data entry simpler and need to figure out a convenient way to edit already entered data.
-}
-- «clientTypes World»
data W = W {
    board            :: Board
  , currentRegion    :: Region
    } deriving (Eq, Read, Show)

data CEvent =
               P PuzzleType Int [Int]  -- Type Size Range
             | F Operation Int         -- Op Value (Op = + - * / etc.)
             | D                       -- Finish region
             | T Int Int               -- Toggle square at x,y
  deriving (Eq, Typeable, Data, Read, Show)
{-
Updaters
Well, there aren't instances in Fay, and there aren't even maps, but as every good lisp programmer knows, you don't really need all that fluff as long as you have lists. Another thing lacking from Fay is the State Monad, so one workaround is to pass the state into every function that needs it. That's where the World comes in. The World type is a reference to the W type defined above. References are defined in the JQBindings code above, and allow you to have mutable variables in the Fay monad. All we need now is a COMMON block and it's just like programming in Fortran all over again. ;-)

So a World is a reference to a W, and and UpdateW is a function that updates a subfield of a W. reverseMap just reverses the relations of a map, turning keys to values and values to keys.
-}
-- «updaters typeSynonyms»
type World = Ref W
type UpdateW a = W -> a -> W


defaultW = W (Board NullP 0 [] [Constraints []]) (R 0 Empty [])

reverseMap :: [(a,b)] -> [(b,a)]
reverseMap = map (\x -> (snd x, fst x))
{-
I need to represent operations three different ways. One as a haskell data type, two as a string that gets put in the value field of an option html element, and three as the text associated with that value that is displayed to the user. These maps allow me to translate from any one of these representations to another. Painful but necessary.
-}
-- «updaters operatorMaps»
valueToOperationMap :: [(String, Operation)]
valueToOperationMap = [
      ("id" , Id)
    , ("plus" , Plus)
    , ("minus" , Minus)
    , ("times" , Times)
    , ("divide", Divide)
    , ("power" , Power)
    , ("mod" , Mod)
    , ("and" , And)
    , ("or" , Or)
  ]

readShowLookup :: Eq a => String -> [(a,b)] -> a -> b
readShowLookup msg l x = maybe (error msg) id $ lookup x l

showOperationForUser :: Operation -> String
showOperationForUser = readShowLookup "No such operation for show" (reverseMap valueToOperationMap)

readOperationForUser :: String -> Operation
readOperationForUser x = readShowLookup ("No such operation for read " ++ x) valueToOperationMap x


valueToOperationTextMap :: [(String, String)]
valueToOperationTextMap = [
      ("", "Select operator for region")
    , ("id" , "=")
    , ("plus" , "+")
    , ("minus" , "-")
    , ("times" , "×")
    , ("divide", "÷")
    , ("power" , "^")
    , ("mod" , "mod")
    , ("and" , "∧")
    , ("or" , "∨")
  ]
operationToTextMap :: [(Operation,String)]
operationToTextMap =
  let
    textOps = 
      map (\x -> maybe (error "Missing op") id
        (lookup x valueToOperationTextMap)) (map fst valueToOperationMap)
    opNames = map snd valueToOperationMap
    in zip opNames textOps
{-
You'ld think I'ld need the same three representations for the PuzzleType data type, but I thought I'ld implement it differently by having the option elements already populate the html file. Just like Perl, tmtowtdi. I also have explanations associated with each PuzzleType that are displayed to the user and stored as hidden elements in the html page.
-}
-- «updaters puzzles»
puzzleMap :: [(String, PuzzleType)]
puzzleMap = [
    ("single" , Single)
  , ("double", Double)
  , ("killer", Killer) ]

reversePuzzleMap :: [(PuzzleType,String)]
reversePuzzleMap = reverseMap puzzleMap

showPuzzle :: PuzzleType -> String
showPuzzle = readShowLookup "No such puzzle type for show" reversePuzzleMap

readPuzzle :: String -> PuzzleType
readPuzzle x = readShowLookup ("No such puzzle type for read " ++ x ) puzzleMap x

underscore = '_'

showCEvent :: CEvent -> String
showCEvent cev = case cev of
  P x1 x2 x3 -> intercalate (underscore:[])
    [ "P" , showPuzzle x1, showInt x2, showList x3 ]
  F x1 x2    -> intercalate (underscore:[])
    [ "F" , showOperationForUser x1, showInt x2]
  T x1 x2    -> intercalate (underscore:[])
    [ "T" , showInt x1, showInt x2]
  D          -> "D"

readCEvent :: String -> CEvent
readCEvent str =
  let a = split underscore str
  in case head a of
      "P" -> P (readPuzzle (a!!1)) (parseInt (a!!2)) (parseIntList (a!!3))
      "F" -> F (readOperationForUser (a!!1)) (parseInt (a!!2))
      "T" -> T (parseInt (a!!1)) (parseInt (a!!2))
      "D" -> D
      otherwise -> error "CEvent not recognized in read"  
                                      
explainMap :: [(PuzzleType, String)]
explainMap = [
      (Single , "#explainCreateRegion")
    , (Double , "#explainCreateRegion")
    , (Killer , "#explainKillerCreateRegion") ]

-- I need to display the different regions in different background colors, so here are the ones that I chose.

-- «updaters colors»
backgroundColorList = [
    "DarkCyan"
  , "DarkGoldenRod"
  , "DarkGreen"
  , "DarkKhaki"
  , "DarkMagenta"
  , "DarkOliveGreen"
  , "Darkorange"
  , "DarkOrchid"
  , "DarkRed"
  , "DarkSalmon"
  , "DarkSeaGreen"
  , "DarkSlateBlue"
  , "DarkSlateGray"
  , "DarkTurquoise"
  , "DarkViolet"
  , "DeepPink"
  , "DeepSkyBlue"
  , "FireBrick"
  , "ForestGreen"
  , "HotPink"
  , "IndianRed"
  , "Indigo"
  , "Maroon"
  , "MidnightBlue"
  , "OrangeRed"
  , "Peru"
  , "RoyalBlue"
  , "SlateGray"
  ]
{-
Here are all the various updaters we need. Makes you appreciate Lenses, which aren't available in Fay.
-}
-- «updaters code»
updateSide :: UpdateW Int
updateSide w v = w { board = (board w) { puzzleSide = v}}
updatePuzzleType :: UpdateW PuzzleType 
updatePuzzleType w v = w { board = (board w) { puzzleType = v}}
updateElementRange :: UpdateW [Int]
updateElementRange w v = w { board = (board w) { puzzleElementRange = v}}

updateLow :: UpdateW Int
updateLow w v = w { board = (board w)
 { puzzleElementRange = [v .. (last . puzzleElementRange . board $ w)]}}
updateHigh :: UpdateW Int
updateHigh w v = w { board = (board w)
 { puzzleElementRange = [(head . puzzleElementRange . board $ w) .. v]}}
updateAll w v = w { board = (board w) { puzzleSide = v, puzzleElementRange = [1..v] }}

updateCurrentRegion :: UpdateW  Region
updateCurrentRegion w v  = w { currentRegion = v}

currentConstraints :: W -> Constraints
currentConstraints = head . puzzleConstraints . board

updateCurrentConstraints :: UpdateW [Region]
updateCurrentConstraints w v =
  let
    b = board w
    newConstraints = Constraints v : (tail . puzzleConstraints $ b)
    newB = b {puzzleConstraints = newConstraints}
  in  w { board = newB }
{-
jQuery Bindings
Even though Fay is Haskell, it is not all of Haskell. In particular, I've found that a lot of read and show instances you take for granted in regular Haskell don't work in Fay. The way around it is to write a bunch of functions with specific types so that the underlying JSON.stringify function does the right thing. Most of this code was ripped from the examples that came with Fay, so I won't go into details here. Also the fay-jquery module continues to change, so please look there to see what is going on. I'll just include a few examples of the functions I defined here, and hide the rest from your gaze.
-}
-- «jqbindings exposed»

data Ref a
instance Show (Ref a)

newRef :: a -> Fay (Ref a)
newRef = ffi "new Fay$$Ref(%1)"

writeRef :: Ref a -> a -> Fay ()
writeRef = ffi "Fay$$writeRef(%1,%2)"

readRef :: Ref a -> Fay a
readRef = ffi "Fay$$readRef(%1)"

parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"

parseIntList :: String -> [Int]
parseIntList = ffi "JSON.parse(%1)"

parseDouble :: Int -> String -> Double
parseDouble = ffi "parseFloat(%2,%1) || 0"

showInt :: Int -> String
showInt = ffi "JSON.stringify(%1)"

showList :: [Int] -> String
showList = ffi "JSON.stringify(%1)"

readList :: String -> [Int] 
readList = ffi "JSON.parse(%1)"

showOperation :: Operation -> String
showOperation = ffi "JSON.stringify(%1)"

readOperation :: String -> Operation
readOperation = ffi "JSON.parse(%1)"

showCoords :: [Coord] -> String
showCoords = ffi "JSON.stringify(%1)"

showRegion :: Region -> String
showRegion = ffi "JSON.stringify(%1)"

showWorld :: W -> String
showWorld = ffi "JSON.stringify(%1)"

readWorld :: String -> W
readWorld = ffi "JSON.parse(%1)"

readBoard :: String -> Board
readBoard = ffi "JSON.parse(%1)"

showEvent :: CEvent -> String
showEvent = ffi "JSON.stringify(%1)"

readEvent :: String -> CEvent
readEvent = ffi "JSON.parse(%1)"

showBoard :: Board -> String
showBoard = ffi "JSON.stringify(%1)"

windowConfirm :: String -> Fay Bool
windowConfirm = ffi "window.confirm(%1)"

alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

simpleClone :: JQuery -> Fay JQuery
simpleClone = ffi "%1['clone']()"

jPost :: String -> Automatic f -> (Automatic g -> Fay ()) -> Fay ()
jPost = ffi "jQuery.ajax(%1, { data: JSON.stringify(%2), type: 'POST', processData: false, contentType: 'text/json', success: %3 })"

jPostBoard :: String -> String -> (String -> Fay ()) -> Fay ()
jPostBoard = ffi "jQuery.ajax(%1, { data: %2, type: 'POST', processData: false, contentType: 'text/json', success: %3 })"

appendString :: String -> JQuery -> Fay JQuery
appendString = ffi "%2['append'](%1)"

windowUrl :: Fay String
windowUrl = ffi "window.location.href"

windowUserAgent :: Fay String
windowUserAgent = ffi "navigator.userAgent"

jsBlur :: JQuery -> Fay ()
jsBlur = ffi "%1['blur']()"

hideIpadKeyboard :: Fay ()
hideIpadKeyboard = ffi "document.activeElement.blur()"
{-
Shared Utils
These are shamelessly copied from Data.List. Since I need them for my Fay code, it thought they might come in hander in the server code too, and there is no reason to to share them.
-}
-- «shared utils 1»
split :: Char -> String -> [String]
split c str = words' (dropWhile isC str)
  where words' []  = []
        words' s = case break isC s of (a,b) -> a : (split c) b
        isC = (==c)

isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys

isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf x y          =  reverse x `isPrefixOf` reverse y

isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

tails                   :: [a] -> [[a]]
tails xs                =  xs : case xs of
                                  []      -> []
                                  _ : xs' -> tails xs'
{-
Miscellaneous Utilities
Everybody needs a module where you stick things that don't really belong anywhere else. Welcome to the Utils module. With Fay you often find yourself stringing together a bunch of jquery calls. The & function is just a rebinding of >>= which is more convenient to use.
debug appends a jquery element to my span identified the the #debug id. It makes it easier to dump stuff to chome when I'm not sure what is being passed back and forth.
selectId just selects the DOM element with the specified id. I got tired of always typeing "#".
-}
-- «utils 1»
(&) :: Fay a -> (a -> Fay b) -> Fay b
x & y = x >>= y
infixl 1 &

debug :: JQuery -> Fay ()
debug x = do
  d <- selectId "debug"
  appendTo d x & void

selectId :: String -> Fay JQuery
selectId str = select ('#' : str)
{-
englishInt does very simple grammatical correctness for nouns whose plural end with s.
splitWith takes a Char and a String and splits the string into a list of strings, removing the Char from the list. Just like Perl's split function.
idFromCoord and coordFromId are inverses. They transforms a Coord type into a string suitable for insertion into the DOM as an id.
void is used often to make the types come out right. Generally in Fay you are returning a Fay (), but sometimes your last statement returns a Fay NotVoid, so just append a & void to the function and you're golden.
-}
-- «utils 2»
englishInt :: Int -> String -> String
englishInt n str =
  case n of
    0 -> "No " ++ str ++ "s"
    1 -> "1 " ++ str
    x -> showInt x ++ " " ++ str ++ "s"

splitWith :: Char -> String -> [String]
splitWith c s =
  case dropWhile (==c) s of
    "" -> []
    s' -> let x = break (==c) s'
         in fst x : splitWith c (snd x)    

idFromCoord :: Coord -> String
idFromCoord (C r c)  = 'T' : intercalate "_" [showInt r, showInt c]

coordFromId :: String -> Coord
coordFromId str =
  let
    digitStrings = splitWith '_' (tail str)
    ints = map parseInt digitStrings
  in C (ints!!0) (ints!!1)

void :: t -> Fay ()
void _ = return ()
{-
doubleQuote - I got tired of escaping everything, and solved it with this simple function.
For some reason mapM isn't included (yet) in Fay's Prelude, so here it is.
exposeIds and hideIds are inverses, and just remove or add the hidden class to a list of elements with the given ids.
The three set functions just set the contents of various spans and divs I've defined. Nothing profound.
I tried this out on an ipad, and the keyboard kept popping up, so I need to see if I'm running on a tablet and disable the checkForEnter feature when inputting the squares.
-}
-- «utils 3»
doubleQuote :: String -> String
doubleQuote s = '"' : s ++ "\""

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM m (x:xs) = m x >>= (\mx -> mapM m xs >>= (\mxs -> return (mx:mxs)))
mapM _ [] = return []

exposeIds :: [String] -> Fay ()
exposeIds l =  forM_ l $ \i -> selectId i & removeClass "hidden"

hideIds :: [String] -> Fay ()
hideIds l =  forM_ l $ \i -> selectId i & addClass "hidden"

setError :: String -> Fay ()
setError msg = do
  statusError <- selectId "statusError"
  case msg of
    "" -> empty statusError & removeClass "error"
    _  -> appendString msg statusError & addClass "error"
  return ()

setExplain :: String -> Fay ()
setExplain explainId = do
  explain <- selectId explainId & contents & simpleClone
  selectId "instructions" & empty & appendJQuery explain & void

setStatus :: String -> String -> Fay ()
setStatus statusId msg = do
  status <- selectId statusId
  setHtml msg status & void

whenTablet :: a -> (a -> Fay ()) -> (a -> Fay ()) -> Fay ()
whenTablet arg trueFun falseFun = do
  agent <- windowUserAgent
  let
    isTablet = "iPhone" `isInfixOf` agent || "iPad" `isInfixOf` agent
  if isTablet then trueFun arg else falseFun arg

{-
The Calcudoku Client
So here is the meat and potatoes of the App. The general idea for a Handler is to update the state of the World based on the Event received, and do whatever Fay actions are necessary. main just waits for the document to be ready, creates a new reference to W, and starts by setting up the size selection widget.

Also I check to see if the query parameter is debug, and if so a couple of addtional text inputs show up on the page at the top. These allow me to input the parsed board or the input log into the textbox and let the server crunch.
-}
-- «client initialize»
type Handler = World -> Event -> Fay ()

main :: Fay ()
main =  ready $ newRef defaultW & initialize

initialize :: World -> Fay ()
initialize world =  do
  d <- isDebug
  let
    sizeRange = if d then [2..15] else [4..15]
  when d (exposeIds ["debug"])
  setupSize world sizeRange


isDebug :: Fay Bool
isDebug = do
  url <- windowUrl
  let
    debug = "debug"
    queryParms = dropWhile (/= '?') $ url
    isDebug = not (null queryParms) && debug == (take (length debug) . tail $ queryParms)
  return isDebug
{-
As the COBOL people like to say, numberOfRegionsDefined and numberOfSquaresToGo are self documenting, so I'll just explain what is going on in updateBoard. I wanted to have all singleton entries, ie cells whose operation is id, be the color white, all cells that aren't part of a region, either existing or being formed, be AliceBlue, cells that are part of the region being created be DeepSkyBlue, and the rest, the already existing regions be various different colors as defined by the backgroundColorList. Additionally, the first cell in every region should contain the operation and value for that region.
-}
-- «client regions»
------------------------------ Regions ------------------------------

numberOfRegionsDefined :: W -> Int
numberOfRegionsDefined = length . regions . currentConstraints

numberOfSquaresToGo :: W -> Int
numberOfSquaresToGo w =
  let
    total = (puzzleSide . board $ w)^2
    alreadyDefined = 
      case  map (length . regionCoords) . regions . currentConstraints $ w of
        [] -> 0
        x -> sum x
    inThisRegion = length . regionCoords . currentRegion $ w
  in total - (alreadyDefined + inThisRegion)

updateBoard :: World -> Fay ()
updateBoard world = do
  w <- readRef world
{-
This code divides the regions into the different classes I want to represent with a distinct background colors. rs is a list of all the exsiting regions. c is a list of lists of the co-ordinates of these regions. singletons filters c and keeps just the co-ordinates that have only length 1 regions. These correspond to regions that are associated to the = (id) operation. multiples are the list of co-ordinates with more than 1 cell in their regions. unmarked are cells that are not part of an existing finished region. Finally, beingDefined are the co-ordinates of the region currently being defined. Each of these types of cells are zipped together with their background color as described above. Finally allRegions is a list of all non-empty regions, even the one that is currently being defined and hasn't yet been completed.
-}
-- «client regions 1»
  let
    rs :: [Region]
    rs = reverse . regions . currentConstraints $ w
    c :: [[Coord]]
    c = map regionCoords rs
    n =  puzzleSide . board $ w
    allCoords = [ C i j | i <- [ 1 .. n] , j <- [1 .. n] ]
    singleTons :: [[Coord]]
    singleTons = filter ((== 1) . length) c
    multiples :: [[Coord]]
    multiples  = filter ((/= 1) . length) c
    unmarked :: [[Coord]]
    unmarked = [filter (\x -> x `notElem` (concat c)) allCoords]
    beingDefined :: [[Coord]]
    beingDefined = [regionCoords . currentRegion $ w]
    white :: [ ([Coord],String) ]
    white = zip singleTons (repeat "White")
    colored :: [ ([Coord],String) ]
    colored = zip multiples (concat . repeat $ backgroundColorList)
    yellow = zip unmarked (repeat "Yellow")
    red = zip beingDefined (repeat "Red")
    allRegions :: [Region]
    allRegions = if null (regionCoords . currentRegion $ w) then rs 
                   else (currentRegion w) : rs
{-
This double loop sets the background color of all the regions. It also resets the attributes and the values of each cell. They will be filled back in later, depending on their type.
-}

-- «client regions 2»
  forM_ (white ++ colored ++ yellow ++ red) $ \i -> do
    let
      cells = fst i
      color = snd i
    forM_ cells $ \j -> do
      cell <- selectCell j
      setAttr "style" ("background-color:" ++ color ++ ";") cell &
        removeClass "cellConstrained"
      setVal "" cell

-- This double loop adds back the cellConstrained class to each region that is already defined and completed.

«client regions 3»
  forM_ (white ++ colored) $ \i -> do
    let
      cells = fst i
      color = snd i
    forM_ cells $ \j -> do
      cell <- selectCell j
      addClass "cellConstrained" cell

-- This loop restores the operation and the value for the first cell in every region.

«client regions 4»
  forM_ allRegions $ \g -> do
    firstCell <- selectCell . (head . regionCoords) $ g
    let
      operator = regionOperation g
      opText = maybe "" id $ lookup operator operationToTextMap
      value = showInt . regionResult $ g
    setVal (value ++ opText) firstCell
  return ()
  where
    selectCell :: Coord -> Fay JQuery
    selectCell c = do
      let cellId = idFromCoord c
      selectId cellId 
{-
doRegionsExist simple checks whether any regions have been defined or are being defined. resetRegions resets the state of the World back to when no regions are yet defined. This is called if the user changes the size or type of the puzzle.
-}
-- «client regions 5»
doRegionsExist :: World -> Fay Bool
doRegionsExist world = do
  w <- readRef world
  let regionsExist = not (null ((regionCoords . currentRegion) w))
                     &&
                     not (null ((regions . currentConstraints) w))
  return regionsExist

resetRegions :: World -> Fay ()
resetRegions world = do
  w <- readRef world
  let
    emptyRegion = R 0 Empty []
    w1 = updateCurrentConstraints w []
    newW = w1 { currentRegion = emptyRegion }
  writeRef world newW
  selectId  "statusRegion" & empty
  setupPuzzleTable world

{-
There is a div in the html file for the current status. This is handy for the user so he can see what is going on, and handy for me to make sure I haven't gone off the rails. rangeStatus display the range of numbers that make up the puzzle, usually from 1 to n. regionStatus displays how many regions have been defined, and how many squares are left to be added to the regions.
-}
-- «client status»
------------------------------ Status ------------------------------

rangeStatus :: World -> Fay ()
rangeStatus world = do
  w <- readRef world
  let
    msg = unwords
      [ "The range runs from" 
       , showInt (head . puzzleElementRange . board $ w)
       , "to"
       ,  showInt (last . puzzleElementRange . board $ w) ]
  setStatus "statusRange" msg

regionStatus :: World -> Fay ()
regionStatus world = do
  w <- readRef world
  let
    d = numberOfRegionsDefined w
    g = numberOfSquaresToGo w
    dText = englishInt d "region"
    gText = englishInt g "square"
    allText = dText ++ " defined, " ++ gText ++ " remaining."
  setStatus "statusRegion" allText
{-
These functions setup the various widgets displayed on the page, and associate them with their handlers. change is a jQuery onchange callback, which is called whenever the status of the widget is changed. There are two little gotchas that are not obvious in this code. One was that to make an option be selected you have to set the selected atrribute to the value "selected." The other is that in the html file, the span associated with the puzzle table must have the attribute contenteditable set to "true."
-}

-- «client setup»
------------------------------ Setup ------------------------------

setupSize :: World -> [Int] -> Fay ()
setupSize world l = do
  selectId "inputWorld"  & click (handleInputWorld world)
  selectId "inputEvents" & click (handleInputEvents world)
  size <- selectId "size"
  change (handleSize world) size
  forM_ l $ \i -> do
    option <- select "<option value=''></option>" & setVal (show i)
    appendTo size option
    setText (show i) option

setupRange :: World -> Int -> Fay ()
setupRange world n = do
  fromSelect <- selectId "from"
  change (handleFrom world) fromSelect
  toSelect <- selectId "to"
  change (handleTo world) toSelect
  add fromSelect toSelect & empty
    -- Note: JQuery is not an instance of Eq
    -- thus the need for the zip junk and the wierd case statement
  forM_ (zip [0..] [fromSelect, toSelect]) $ \(j,div) ->
    forM_ [-n .. n] $ \i -> do
       option <- select "<option value=''></option>" 
       setVal (show i) option
       let selected = case j of
             0 -> if i == 1     then setAttr "selected" "selected" else return
             1 -> if i == n then setAttr "selected" "selected" else return
       s <- selected option
       appendTo div s
       setText (show i) s

setupPuzzleTable :: World -> Fay ()
setupPuzzleTable world = do
  w <- readRef world
  let n =  puzzleSide . board $ w
  table <- select "<table border='1' style='float:left'></table>"
  p <- selectId "puzzleTable" 
  empty p & appendJQuery table 
  whenTablet p void (keyup (checkForEnter world))
  let
    rowColumn :: [[[Int]]]
    rowColumn = [ [ [i,j] | j<-[1..n] ] | i<-[1..n] ]
  forM_ rowColumn $ \row -> do
    tr <- select "<tr></tr>" & appendTo table
    forM_ row $ \ij -> do
      let c = C (ij!!0) (ij!!1)
      td <- select "<td></td>" & appendTo tr
      let button = unwords [
                  "<input type='button'"
                , "id='" ++ idFromCoord c ++ "'"
                , "class='tableCell'>" ]
      select button & appendTo td
                    & click (handleToggleSquare world)
  updateBoard world
{-
Here all the various callback handlers are defined. worldChangeHandler takes one of two actions, depending upon whether regions exist or not. This is used when the user decides to change a parameter of the puzzle that would affect the currently existing regions. An alert window is presented to give the user a chance to change his mind. If he goes ahead, all existing regions are reset. backoutWithWorld resets the value of the widget associated with an event to a string that is computed based on the current state of the world.
-}
-- client handlers 1»
------------------------------ Handlers ------------------------------

worldChangeHandler :: World -> Event -> Handler -> Handler -> Fay ()
worldChangeHandler world e h1 h2 = do
  ok <- doRegionsExist world
  handler <- if ok then do
    msg <- selectId "worldChange" & getHtml
    ok <- windowConfirm msg
    return $ if ok then h1 else h2
    else return h1
  handler world e

backoutWithWorld :: (W -> String) -> Handler  
backoutWithWorld f world e = do
  w <- readRef world
  let v = f w
  t <- target e
  selectElement t & setVal v
  return ()
{-
handleSize takes care of two cases. changeSize is called the first time the size is defined and resetSize is called when the size is changed after some regions have already been defined. Once the size is specified, we are ready to let the user define the range of values to be allowed in the puzzle, so we populate and display the "from" and "to" range select widgets based on the size. At this point we can expose the range widget, the puzzleType widget, and the puzzleTable table. The operation and value remain hidden until the puzzleType is known, since if the puzzleType is Killer thexn there is no need for the operation select widget, as it is always Plus. Also the instructions div set to explain to the user what to do next. The way this works is to copy the contents of a hidden div in the html file to the instructions div. The doParm function handles updating the state of world based on a parser, an updater, and an event.
-}

-- «client handlers 2»
handleSize :: Handler
handleSize world e = worldChangeHandler world e changeSize resetSize 
  where
    changeSize world e = do
      haveRegions <- doRegionsExist world
      when haveRegions  (resetRegions world >> initialize world)
      doParam world parseInt updateAll e
      w <- readRef world
      let
        pSize = puzzleSide . board $ w
        n = showInt pSize
        msg = "The size of the puzzle is " ++ n ++ " by " ++  n
      setStatus "statusSize" msg
      setupRange world pSize
      puzzleType <- selectId "puzzleType"
      change (handlePuzzleType world) puzzleType
      setExplain "explainRange"
      rangeStatus world
      exposeIds ["range"]
      setupPuzzleTable world
    resetSize = backoutWithWorld (showInt . puzzleSide . board)

{-
handleFrom and handleTo are pretty straightforward. handlePuzzleType is again more complicated because if it is called after regions have been defined, we must warn the user of the consequences of changing the type, namely that his regions will be lost. At this point we set up the Operation widget and if the puzzleType is Killer, we hide it, otherwise we expose it. We also set up the instructions div to tell the user what to do next.

«client handlers 3»
-}

handleFrom :: Handler
handleFrom world e = do
  doParam world parseInt updateLow e
  rangeStatus world

handleTo :: Handler
handleTo world e = do
  doParam world parseInt updateHigh e
  rangeStatus world

handlePuzzleType :: Handler
handlePuzzleType world e =
  worldChangeHandler world e changePuzzleType resetPuzzleType 
  where
    changePuzzleType world e = do
      haveRegions <- doRegionsExist world
      when haveRegions (resetRegions world)
      let parsePuzzle x = maybe (error "No such puzzle type") id (lookup x puzzleMap)
      doParam world parsePuzzle updatePuzzleType e
      w <- readRef world
      let bd = board w
      logEvent (P (puzzleType bd) (puzzleSide bd)(puzzleElementRange bd) )
      operator <- selectId "operator" & empty
      forM_ valueToOperationTextMap $ \vt -> do
        option <- select "<option></option>" & appendTo operator
        setVal (fst vt) option
        setHtml (snd vt) option
      if (puzzleType . board $ w) == Killer then do
        hideIds ["operator"]
        setExplain "explainKillerCreateRegion"
        else do
          exposeIds ["operator"]
          setExplain "explainCreateRegion"
      selectId "finishRegion"  & click (handleFinishRegion world)
      selectId "discardRegion" & click (handleDiscard world)
      exposeIds ["resultSpan" , "finish" ]
      rangeStatus world & void
    toPuzzleString x = maybe "single" id (lookup x reversePuzzleMap)
    resetPuzzleType = backoutWithWorld (toPuzzleString . puzzleType . board)

{-
handleDiscard is run when the user clicks on the discard last region button. It simply deletes the most recently defined region. Newer regions are appended to the front of the Constraints list, so the is basically a Haskell tail function. The World and the various html widgets are updated to reflect this.

handleToggleSquare has to do a bunch of things. During the setup, coordId contains the DOM id of the cell, thisCoord contains the Coord value of this cell, operator the widget value of the operation select widget associated with this region, and r1 the widget value of the resulting value for the this region. Next we check that these are actually defined, and alert the user if they are not. If both are defined, we create a new R (region) value, and toggle the membership of this cell. That means if that cell is already part of the region being defined, it is removed, otherwise it is added. We finish the region if we can (operation is id or mod) and update the board and the various status widgets to reflect what has happened. If there are no squares left, we also finish this region.

«client handlers 4»
-}

safeTail :: [a] -> [a]
safeTail l = if null l then l else tail l

handleDiscard :: Handler
handleDiscard world _ = do
  w <- readRef world
  let newW = updateCurrentConstraints w (safeTail . regions . currentConstraints $ w)
  writeRef world newW
  logEvent D
  updateBoard world
  regionStatus world

handleToggleSquare :: Handler
handleToggleSquare world e = do
  {- When I finally tried this on my ipad, I discovered that every time
     I touched a square in the puzzle, the keyboard would pop up.  I 
     tried not attaching the keyup handler to the table, but that had 
     no effect, so after searching around discovered I could blur the
     active element and that would push the keyboard back down into
     its place. 
  -}
  whenTablet () (const hideIpadKeyboard) void
  w <- readRef world
  tableCell <- target e & selectElement
  coordId <- getAttr "id" tableCell
  let thisCoord = coordFromId coordId
  let pType = puzzleType . board $ w
  if pType == NullP then setError "You must select a puzzle type!" else do
    operator <- case puzzleType . board $ w of
      Killer -> return "plus"
      otherwise -> selectId "operator" & getVal
    let
      op = lookup operator valueToOperationMap
    result <- selectId "result"
    r1 <- getVal result
    setError ""
    case op of
      Nothing -> setError "You must select an operator!"
      Just theOperator -> do
        setError ""
        case r1 of
          "" -> setError "You must select a value!"
          r -> do
              setError ""
              let g = R (parseInt r) theOperator (regionCoords . currentRegion $ w)
                  newR = toggleMembership thisCoord g
                  newW = w { currentRegion = newR }
              writeRef world newW
              logEvent (T (cRow thisCoord) (cColumn thisCoord) )
              case length (regionCoords newR) of
                1 -> when (theOperator ==  Id) $ handleFinishRegion world undefined
                2 -> when (theOperator == Mod) $ handleFinishRegion world undefined
                otherwise -> return ()
              regionStatus world
              updateBoard world
              let squaresLeft = numberOfSquaresToGo newW
              when (squaresLeft == 0) $ handleFinishRegion world undefined

{-
As you might guess, we get here when a region has been finished. At this point we have to add the current region to the list of Constraints, and reset the current region to null. We also check to see if there are any more squares left to be defined. If not we need to do more finishing.

«client finishRegion»
-}

logEvent :: CEvent -> Fay ()
logEvent e = do
  puzzleEvents <- selectId "puzzleEvents"
  appendString (' ' : showCEvent e ) puzzleEvents & void

handleFinishRegion :: World -> t -> Fay ()
handleFinishRegion world _ = do
  w <- readRef world
  let
    g = currentRegion w
    c = regions . currentConstraints $ w
    w1 = updateCurrentConstraints w (g:c)
    newW = w1 { currentRegion = (R 0 Empty [])}
  writeRef world newW
  logEvent (F (regionOperation g) (regionResult g) )
  selectId "operator" & setVal ""
  selectId "result" & setVal ""
  updateBoard world
  let squaresLeft = numberOfSquaresToGo newW
  regionStatus world
  -- puzzleRegions <- selectId "puzzleRegions" & empty
  -- setHtml (showBoard . board $ newW) puzzleRegions
  -- putStrLn $ "finishRegion " ++ showWorld newW
  when (squaresLeft == 0) $ handleAllCellsDefined world

{-
handleAllCellsDefined is called whenever there are no more squares left to define. That does not necessarily mean we are completely finished with the puzzle. Some calcudoku puzzles are Doubles, meaning that the same solution must exist for two different puzzles. We check for this and start over if we have just finished defining the first part of a Double. Once the puzzle is defined completely, we set the answer region to a message that the answer should appear here shortly. This is in case the puzzle takes too long to solve. We then run an ajax call to the server with the Board as the posted data. If all goes well, the server responds with a solution which is put into the answer div. The answer returned should be plain text that is stuffed into an pre element. Just for fun, we also display the data sent to the server in the post request in the puzzleData div. I found this useful for debugging, and perhaps if the user is a programmer it will help them understand what is going on.

«client handleAllCellsDefined»
-}

handleAllCellsDefined :: World -> Fay ()
handleAllCellsDefined world = do
  w <- readRef world
  if (puzzleType . board $ w) == Double then
     case (length . puzzleConstraints . board $ w) of
       1 -> do
         let
           b = board w
           newB = b { puzzleConstraints = (Constraints []) : puzzleConstraints b}
           newW = w { board = newB }
         writeRef world newW
         setExplain "secondPartOfDouble"
         hideIds ["size", "puzzleType", "range" ]
         setupPuzzleTable world
--         putStrLn $ "finishPuzzle " ++ showWorld newW
       2 ->  setExplain "doubleFinished" >> postToSnap w
       _ -> error "there should only be 1 or 2 puzzle constraints"
     else do
      postToSnap w
postToSnap :: W -> Fay ()
postToSnap w = do
  let bd = board w
  setExplain "puzzleFinished"
  selectId "answer" & empty & setHtml "The answer should appear here shortly"
  let
    cleanConstraints = map removeEmptyRegions (puzzleConstraints bd)
    removeEmptyRegions c = Constraints $
                          filter (\g -> regionOperation g /= Empty) (regions c)
    newB = bd { puzzleConstraints = cleanConstraints }
  puzzleData <- selectId "puzzleData" & empty
  let
    sb = showBoard newB
    lsb = length sb
    t1 = "<hr/><br/>This is the data that is being sent to the server, if you have problems, please copy and paste it in a message to Henry.<br/>"
    t2 = "<br/>Post length is: " ++ (showInt lsb) ++ " bytes</p>"
  -- setHtml (intercalate " " [t1,sb,t2])  puzzleData
  -- setHtml (intercalate " " [t1,sb])  puzzleData
  jPost "fayparse" newB setAnswer
  
setAnswer :: String -> Fay ()
setAnswer s = do
  answer <- selectId "answer" & empty
  append s answer & void

{-
doParm is a little tricky. It is called whenever an event happens that affects the state of the World. The parser arguement is applied to the value of the element that is associated with the event. It should return some type that will be used to update the state of the World, such as an Int, a Operation, a Value, etc. The updateF function will be one of the functions defined in Updaters module. It does the actual updating, and returns a new W, which is then written back into the World global. This is an excellant place to watch what is going on, hence the commented out putStrLn which writes the new World to the console.

«client doparm»
-}----------------------------- Others ------------------------------

doParam :: World -> (String -> a) -> UpdateW a -> Event -> Fay ()
doParam world parser updateF e = do
  t <- target e
  sval <- selectElement t & getVal 
  w <- readRef world
  let newW = updateF w (parser sval)
  writeRef world newW
--  putStrLn $ "doParam " ++ showWorld newW

{-
checkForEnter is an event handler that checks to see if the enter key has been pressed. Pressing the enter key means that the current region being defined is finished. It is easier to do this rather than clicking on the "finish region" button.
-}

toggleMembership toggle a cell's membership in a Region, that is all.

«client others»
checkForEnter :: Handler
checkForEnter world e = do
  code <- which e
  -- putStrLn $ "checkForEnter: " ++ (showInt code)
  when (code == 13) $ handleFinishRegion world e

toggleMembership :: Coord -> Region -> Region
toggleMembership c g =
  if c `elem` regionCoords g then g { regionCoords = filter (/= c) (regionCoords g) }
    else g { regionCoords = regionCoords g ++ [c]}

runEvents :: String -> Fay ()
runEvents theEvents = do
  let
    ww = (W (Board Single 0 [] [Constraints []]) (R 0 Empty []))
    finalW = foldl updateW ww (map readCEvent . words $ theEvents)
    updateW :: W -> CEvent -> W 
    updateW w cev =
      let bd = board w
      in case cev of
        P x1 x2 x3 -> w { board =
                            bd {puzzleType = x1,
                                puzzleSide = x2,
                                puzzleElementRange = x3}}
        F x1 x2 -> 
          let
            g = R x2 x1 (regionCoords . currentRegion $ w)
            c = regions . currentConstraints $ w
            w1 = updateCurrentConstraints w (g:c)
            newW = w1 { currentRegion = (R 0 Empty [])}
          in newW
        D ->  updateCurrentConstraints w 
               (safeTail . regions . currentConstraints $ w)
        T x1 x2 ->
          let r = currentRegion w
              c = C x1 x2
              newR = toggleMembership c r
          in w { currentRegion = newR }
  -- putStrLn $ "runEvents " ++ showWorld finalW
  postToSnap finalW

handleInputWorld :: Handler
handleInputWorld _ _ = do
  worldText <- selectId "worldText" & getVal
  let newB = readBoard worldText
      newW = W newB (R 0 Empty [])
  postToSnap newW

handleInputEvents :: Handler
handleInputEvents _ _ = do
  worldEvents <- selectId "worldEvents" & getVal
  runEvents worldEvents

{-
Conclusion
Well, that is all for now. Next I'll try to document the Solver, for your amusement and edification. I hope you've enjoyed this tour through a sample Fay application. Thank you again, Chris and Adam for allowing me to escape the Javascript jail and enter the Haskell heaven.

-}