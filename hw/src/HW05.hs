{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

import Data.Bits (xor)
import Data.List (sortBy)

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret p1 p2 = do
    bs1 <- BS.readFile p1
    bs2 <- BS.readFile p2
    let up1 = BS.unpack bs1
    let up2 = BS.unpack bs2
    let z = zip up1 up2
    let comp = [t | (x, y) <- z , let t = xor x y, t /= 0]
    return (BS.pack comp)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey bs path = do
    let keyCycle = cycle (BS.unpack bs)
    bsEnc <- BS.readFile (path ++ ".enc")
    let z = zip (BS.unpack bsEnc) keyCycle
    let comp = [t | (x, y) <- z, let t = xor x y, t /= 0]
    BS.writeFile path (BS.pack comp)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
    bs <- BS.readFile path
    return (decode bs)


-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vpath tpath = do
    vlist <- parseFile vpath :: IO (Maybe [TId])
    tlist <- parseFile tpath :: IO (Maybe [Transaction])

    let newlist = case tlist of
            Nothing -> Nothing
            Just x  -> case vlist of
                         Nothing -> Nothing
                         Just y  -> Just (filter chk x)
                                    where
                                      chk :: Transaction -> Bool
                                      chk tr = tid tr `elem` y

    return newlist

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow tr = getAccFlowTo (getAccFlowFrom Map.empty tr) tr --[(from, (- flow)) (to, flow) | (from, to, amount, _) <- tr]
      where
        getAccFlowFrom m []  = m
        getAccFlowFrom m (x:trs)
          | not (Map.member (from x) m) = getAccFlowFrom (Map.insert (from x) (- (amount x)) m) trs
          | otherwise = getAccFlowFrom (Map.insert (from x) ((m Map.! (from x)) - (amount x)) m) trs
        getAccFlowTo m []  = m
        getAccFlowTo m (x:trs)
          | not (Map.member (to x) m) = getAccFlowTo (Map.insert (to x) (amount x) m) trs
          | otherwise = getAccFlowTo (Map.insert (to x) ((m Map.! (to x)) + (amount x)) m) trs


-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m  = fst (Map.foldrWithKey f ("", -1) m)
    where
    f name1 val1 (name2, val2)
      | val1 >= val2 = (name1, val1)
      | otherwise = (name2, val2)

test = do
    trs <- (getBadTs "src/victims.json" "src/transactions.json")
    let ret = case trs of
                Nothing -> error "123"
                Just x -> x
    return (getCriminal (getFlow ret))

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = acc (sortPay (> 0)) (sortPay (< 0)) tids []
  where
    acc :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction] -> [Transaction]
    acc [] _ _ trs       = trs
    acc _ [] _ trs       = trs
    acc (x:payer) (y:payee) (t:ttid) trs
      | (snd x) + (snd y) == 0 = acc payer payee ttid ((Transaction (fst x) (fst y) (snd x) t):trs)
      | (snd x) + (snd y) >  0 = acc ((fst x, snd x + snd y):payer) payee ttid ((Transaction (fst x) (fst y) (-(snd y)) t):trs)
      | otherwise              = acc payer ((fst y, snd x + snd y):payee) ttid ((Transaction (fst x) (fst y) (snd x) t):trs)
    absdes x y = (abs $ snd x) > (abs $ snd x)
    sortPay f = sortBy absdes $ filter f . snd $ toList m


-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

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
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

