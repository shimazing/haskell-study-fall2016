{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.List
import Parser


xorByte :: ByteString -> ByteString -> ByteString
xorByte bs1 bs2 = BS.pack (map (uncurry xor) $ zip ba1 ba2)
  where
    ba1 = BS.unpack bs1
    ba2 = BS.unpack bs2

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret fileKeyHidden fileOrg = do
    keyFile <- BS.readFile fileKeyHidden
    oriFile <- BS.readFile fileOrg
    return $ BS.pack $ filter (/= 0) $ BS.unpack $ xorByte keyFile oriFile

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filePath = do
    let encrypted = filePath ++ ".enc"
    encFile <- BS.readFile encrypted
    let encFileByte = BS.unpack encFile
    let keyByte = BS.unpack key
    let decrypted = (BS.pack (map (uncurry xor) $ zip encFileByte (cycle keyByte)))
    BS.writeFile filePath decrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
    str <- BS.readFile file
    return $ decode str

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vicFile tsFile = do
    vicList_ <- parseFile vicFile
    tsList_ <- parseFile tsFile
    let vicList = fromMaybe [] vicList_
    let tsList = fromMaybe [] tsList_
    return $ Just $ filter (\x -> elem (tid x) vicList) tsList

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow ((Transaction from to amount _):xs)
    | Map.member from prevMap && Map.member to prevMap
        = Map.adjust (\x -> x + amount) to $ Map.adjust (\x -> x - amount) from prevMap
    | Map.member from prevMap
        = Map.insert to amount $ Map.adjust (\x -> x - amount) from prevMap
    | Map.member to prevMap
        = Map.insert from (negate amount) $ Map.adjust (\x -> x + amount) to prevMap
    | otherwise
        = Map.insert from (negate amount) $ Map.insert to amount prevMap
  where
    prevMap = getFlow xs

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal transMap = fst $ head $ sortOn (\(_, y) -> -y) $ Map.toList transMap

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs transMap tidList = helper payers payees tidList
  where
    payers = sortOn (\(_,y) -> -y) $ filter (\(_, y) -> y > 0) $ Map.toList transMap
    payees = sortOn (\(_,y) -> y) $ filter (\(_, y) -> y < 0) $ Map.toList transMap
    helper :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
    helper [] [] _ = []
    helper ((k1, v1):xs) ((k2, v2):ys) (z:zs)
        | v1 > (negate v2)
            = [Transaction k1 k2 (negate v2) z] ++ (helper ((k1, v1+v2):xs) ys zs)
        | v1 < (negate v2)
            = [Transaction k1 k2 v1 z] ++ (helper xs ((k2, v1+v2):ys) zs)
        | otherwise
            = [Transaction k1 k2 v1 z] ++ (helper xs ys zs)

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path content = BS.writeFile path $ encode content

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "HW05/clues/dog-original.jpg"
                        "HW05/clues/dog.jpg"
                        "HW05/clues/transactions.json"
                        "HW05/clues/victims.json"
                        "HW05/clues/new-ids.json"
                        "HW05/clues/new-transactions.json"
  putStrLn crim

