import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.List

parser = many (decimal <* char '\n')

reallyParse p bs = case parse p bs of
    Partial f -> f BS.empty
    v -> v

--import Control.Monad.Random

fastRead :: String -> Int
fastRead s = case readDec s of [(n, "")] -> n

--Bare metal raw speed

fastparse = do
    numbers <- BS.readFile "data"
    case reallyParse parser numbers of
        Done t r | BS.null t -> writeFile "sorted" . unlines . map show . sort $ r
