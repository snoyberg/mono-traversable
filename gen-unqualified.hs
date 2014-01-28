import Data.List
import Data.Maybe
import Control.Monad.Trans.Writer
import Control.Monad

main :: IO ()
main = do
    orig <- readFile "Data/Conduit/Combinators.hs"
    let ls = takeWhile (not . isImport) $ drop 1 $ dropWhile (/= starting) $ lines orig
    let (x, y) = runWriter $ mapM process ls
    putStrLn "{-# LANGUAGE NoMonomorphismRestriction #-}"
    putStrLn "module Data.Conduit.Combinators.Unqualified"
    mapM_ putStrLn x
    putStrLn "import qualified Data.Conduit.Combinators as CC"
    forM_ y $ \(old, new) -> do
        putStrLn ""
        putStrLn $ "-- | See 'CC." ++ old ++ "'"
        putStrLn $ new ++ " = CC." ++ old
        putStrLn $ "{-# INLINE " ++ new ++ "#-}"

starting = "module Data.Conduit.Combinators"
isImport = ("import " `isPrefixOf`)

process l
    | null rest || head rest `elem` "(-)" = return l
    | otherwise = do
        let orig =
                case dropWhile (/= '.') rest of
                    '.':orig' -> orig'
                    _ -> rest
            newName = tweak orig
        tell [(orig, newName)]
        return $ lead ++ newName
  where
    (lead, rest) = span (`elem` " ,") l

tweak orig
    | any (`isPrefixOf` orig) (words "yield source sink conduit") = orig
    | otherwise =
        case reverse orig of
            'E':orig' -> reverse orig' ++ "CE"
            _ -> orig ++ "C"
