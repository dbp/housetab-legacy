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

-- ripped out of snap-extension-mongodb
addTimeStamps d = do
  t <- getCurrentTime
  let tsc = ["created_at" =: t]
  let tsu = ["updated_at" =: t]
  return $ tsu `merge` d `merge` tsc


newUser :: Service s => ConnPool s -> String -> [String] -> IO (Maybe User)
newUser p name emails = do
  token <- getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
  temppass <- getStdGen >>= return . B8.pack . take 15 . randomRs ('a','z')
  SaltedHash (Salt s) pwd' <- buildSaltAndHash temppass
  let (newsalt, newpass)  = (BS.pack s, BS.pack pwd')
  let user = (User emptyAuthUser { userPassword = Just (Encrypted newpass), userSalt = Just newsalt } 
                   (B8.pack name) 
                   (map B8.pack emails) 
                   emptyResult 
                   (Just token) 
                   Nothing)
  d <- addTimeStamps $ authUserToDoc $ authUser user
  let d' = d `merge` (additionalUserFields user)
  mid <- runDB p $ insert "users" d'
  case mid of
    Left err -> return Nothing
    Right id' ->
      case cast' id' of
        Nothing -> return Nothing
        Just id' -> return $ Just $ user {authUser = (authUser user) {userId = Just id'}}

createShare (d,v) = Share (read d) v

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
                  return $ HouseTabEntry Nothing htid who (B8.pack what) "misc" (read when) (read howmuch) whopays
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
  