{-# LANGUAGE OverloadedStrings #-}

import            Snap.Extension.DB.MongoDB hiding (lookup)
import qualified  Data.Bson as B
import qualified  Data.Map as M
import            Snap.Auth
import qualified  Data.ByteString as BS
import            Data.ByteString.Internal
import qualified  Data.ByteString.Char8 as B8
import            Data.UString ()  -- Show and IsString instances of UString
import            Data.Maybe
import            Data.Time.Clock
import            Numeric
import            Codec.Utils

import            System.Random
import            System.Environment (getArgs)
                  
import            Data.Digest.SHA512

import Models.Entry
import Models.Account
import Models.Person
import Models.Result

{- 
This module is a bit of a mess, for reasons:
    1. I need to reach inside of snap-auth for mongodb, which I can only really do by duplicating hidden functions, because I need to be able to programmatically create new users, which isn't really possible through the public API.
    2. This is something that only needs to happen once. This is truly throwaway code - it exists to get data out of one legacy format into a new format (specifically, from a custom dump that is generated from happstack-state and into mongodb). It will only need to be used once (after being tested a few times), it will not need to be improved upon, as if I need to do something similar again, I can just write new code to do it. 
    3. The target is an empty database, so the cost of screwing up is very low - a mistake simply means droping the database, fixing tho code, and running it again.
    4. The data set is quite small, so iterating as described it 3 is totally feasible.
    5. It could be worse - this is probably the most reliable imperative code I've ever written - once I got it to compile, it worked correctly, the first time. That's not bad :)
-}


type IAccountName = String
type IEmail = String
type IId = String
type IWho = String
type IWhat = String
type IWhen = String
type IHowmuch = String
type IWhopays = String
type IEntry = (IId,IWho,IWhat,IWhen,IHowmuch,IWhopays)
type IName = String
type ILetter = Char
type IDate = String
type IShare = Double
type IPerson = (IName,ILetter,[(IDate,IShare)]) 

type DumpFormat = (IAccountName,[IEmail],([IEntry],[IPerson]))

runDB p r = access safe Master p (use (Database "housetab") r)

hashFunc a = (iterate hash a) !! 512 -- this is defaultHash for snap-auth

-- next two types and three functions ripped out of Snap.Auth.Password, and hardcoded a little
newtype Salt = Salt { unSalt :: [Octet] }
data SaltedHash = SaltedHash 
  { shSalt :: Salt
  , shHash :: [Octet]
  }
randomSalt :: IO Salt
randomSalt = do
    chars <- sequence $ take 16 $ repeat $
        randomRIO (0::Int,15) >>= return . flip showHex ""
    return $ Salt $ map c2w $ concat chars
buildSaltAndHash :: BS.ByteString -> IO SaltedHash
buildSaltAndHash str = do
    salt <- randomSalt
    return $ hashPassword salt str
hashPassword :: Salt -> BS.ByteString -> SaltedHash
hashPassword s pwd = SaltedHash s h
  where h = hashFunc ((unSalt s) ++ pwd')
        pwd' = BS.unpack pwd

-- | ripped out of snap-extension-mongodb
addTimeStamps d = do
  t <- getCurrentTime
  let tsc = ["created_at" =: t]
  let tsu = ["updated_at" =: t]
  return $ tsu `merge` d `merge` tsc


-- | a lot of this comes right out of snap-auth, but it combines a bunch of separate steps into one
newUser :: Service s => ConnPool s -> String -> [String] -> IO (Maybe User)
newUser p name emails = do
  token <- getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
  temppass <- getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
  SaltedHash (Salt s) pwd' <- buildSaltAndHash temppass
  let (newsalt, newpass)  = (BS.pack s, BS.pack pwd')
  let user = (User emptyAuthUser { userPassword = Just (Encrypted newpass), userSalt = Just newsalt } 
                   (B8.pack name) 
                   (B8.pack $ head emails)
                   Nothing
                   emptyResult 
                   (Just token) 
                   Nothing
                   True
                   True)
  d <- addTimeStamps $ authUserToDoc $ authUser user
  let d' = d `merge` (additionalUserFields user)
  mid <- runDB p $ insert "users" d'
  case mid of
    Left err -> return Nothing
    Right id' ->
      case cast' id' of
        Nothing -> return Nothing
        Just id' -> return $ Just $ user {authUser = (authUser user) {userId = Just id'}}

createShare (d,v) = Share (swapDate (read d)) v

swapDate (Date y m d) = Date m d y

unDoc (Doc fields) = fields
processNew fields = if isNothing (B.lookup "_id" fields :: Maybe ObjectId) then exclude ["_id"] fields else fields
      
createPerson :: Service s => ConnPool s -> B.ObjectId -> IPerson -> IO (Maybe (ILetter, B.ObjectId))
createPerson p htid (name,letter,shares) = do
  let person = Person Nothing htid (B8.pack name) (map createShare shares)
  id' <- runDB p $ insert "people" (processNew $ unDoc $ val person)
  case id' of
    Left err -> return Nothing
    Right id'' ->
      case cast' id'' of
        Nothing -> return Nothing
        Just pid -> return $ Just $ (letter, pid)

createEntry :: Service s => ConnPool s -> B.ObjectId -> [(ILetter, B.ObjectId)] -> IEntry -> IO (Maybe HouseTabEntry)
createEntry p htid people (id,who',what,when,howmuch,whopays') = do
  let entry' = do who <- lookup (head who') people
                  whopays <- sequence (map (flip lookup people) whopays')
                  return $ HouseTabEntry Nothing htid who (B8.pack what) "misc" (swapDate (read when)) (read howmuch) whopays
  case entry' of
    Nothing -> return Nothing
    Just entry -> do
      id' <- runDB p $ insert "entries" (processNew $ unDoc $ val entry)
      case id' of
        Left err -> return Nothing
        Right id'' ->
          case cast' id'' of
            Nothing -> return Nothing
            Just eid -> return $ Just $ entry {eId = eid}
  
impDump :: Service s => ConnPool s -> DumpFormat -> IO ()
impDump p (name,emails,(entries,people)) = do
  acnt <- newUser p name emails
  case acnt >>= (userId . authUser) >>= (bs2objid.unUid) of
    Nothing -> putStrLn "Couldn't create user account"
    Just htid -> do
      insertedPeople <- fmap catMaybes $ mapM (createPerson p htid) people 
      insertedEntries <- fmap catMaybes $ mapM (createEntry p htid insertedPeople) entries
      putStrLn $ 
        (if (length insertedPeople == length people) && (length insertedEntries == length entries) 
        then "SUCCESS: "
        else "FAILURE: ") 
        ++ name ++ ": " 
        ++ (slen insertedPeople) ++ "/" ++ (slen people) ++ " people, " 
        ++ (slen insertedEntries) ++ "/" ++ (slen entries) ++ " entries."     
 where slen = show . length

main = do
  pool <- newConnPool 1 (host "127.0.0.1")
  file <- fmap (head . drop 1 . dropWhile (/= "--file")) getArgs
  dump <- readFile file
  mapM_ (impDump pool) $ map read (lines dump) 
  