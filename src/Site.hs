{-# LANGUAGE OverloadedStrings #-}

module Site
  ( site
  ) where

import            Control.Applicative
import            Control.Monad
import            Data.Maybe
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Snap.Extension.Heist
import            Snap.Util.FileServe
import            Snap.Types
import            Text.Templating.Heist
import            Snap.Auth
import            Snap.Auth.Handlers
import qualified  Data.Bson as B
import            Snap.Extension.DB.MongoDB hiding (index, label)
import qualified  Data.ByteString.Char8 as B8
import            Text.Digestive.Types
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import            Text.Digestive.Validate
import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import            Data.List (null)

import            Application
import            Account
import            State
import            Lib

index :: Application ()
index = do  u <- currentUser
            let name = TE.decodeUtf8 $ maybe "No User" accountName u
            ifTop $ (heistLocal $ (bindString "user" name)) $ render "index"

errorP :: String -> Application ()
errorP msg = (heistLocal $ (bindString "message" (T.pack msg))) $ render "error"

renderPersonResult :: Monad m => (Person,Spent,Owes) -> Splice m
renderPersonResult (person,spent,owes) = do
  runChildrenWithText [("personName", TE.decodeUtf8 $ name person)
                      ,("personLetter", T.pack $ [letter person])
                      ,("personSpent",  T.pack $ show spent)
                      ,("personOwes",  T.pack $ show owes)]
                       
renderResult :: Monad m => Result -> Splice m
renderResult (Result people date) = mapSplices renderPersonResult people

renderEntry :: Monad m => (Int, HouseTabEntry) -> Splice m
renderEntry (index, (HouseTabEntry who what when howmuch whopays)) = do
  runChildrenWithText [("index",       T.pack $ show index)
                      ,("entryBy",     TE.decodeUtf8 who)
                      ,("entryWhat",   TE.decodeUtf8 what)
                      ,("entryDate",   T.pack $ show when)
                      ,("entryAmount", T.pack $ show howmuch)
                      ,("entryFor",    TE.decodeUtf8 whopays)
                      ]
                       
renderEntries :: Monad m => [HouseTabEntry] -> Splice m
renderEntries entries = mapSplices renderEntry (zip [0..] entries)

                     
entriesH :: Application ()
entriesH = do u <- currentUser
              case u of
                Nothing -> errorP "No User"
                Just user ->
                  (heistLocal $ (bindSplices splices)) $ render "entries"
                    where splices = [ ("result",  (renderResult  $ currentResult user))
                                    , ("entries", (renderEntries $ houseTabEntries user))
                                    ]

personCheck :: Validator Application Html Person
personCheck = check "Shouldnt see this" $ \(Person n l ps) -> True

nonEmpty :: Validator Application Html String
nonEmpty = check "String must not be empty." $ \s -> not $ null s
    
lenOne :: Validator Application Html String
lenOne = check "String must be a single character." $ \s -> length s == 1

addPersonForm :: SnapForm Application Html BlazeFormHtml Person
addPersonForm = mkPerson
    <$> label "Name: "    ++> inputText Nothing `validate` nonEmpty <++ errors
    <*> label "Letter: "  ++> inputText Nothing `validate` lenOne <++ errors
  where mkPerson n l = Person (B8.pack n) (head l) []

addPerson :: Application ()
addPerson = do u <- currentUser
               case u of
                Nothing -> errorP "No User"
                Just user -> do
                  r <- eitherSnapForm addPersonForm "add-person-form"
                  case r of
                      Left form' -> 
                        heistLocal (bindSplice "formdata" (return $ docContent $ renderHtml $ fst $ renderFormHtml form')) $ render "form"
                      Right person' -> do
                        let u' = user {houseTabPeople = (houseTabPeople user) ++ [person']}
                        let u'' = u' {currentResult = run (houseTabPeople u') (houseTabEntries u')}
                        saveAuthUser (authUser u'', additionalUserFields u'')
                        redirect "/entries"

onePerson :: Validator Application Html String
onePerson = checkM "Must be a single letter that corresponds to a person." fn
  where fn p = do peop <- currentPeople
                  case peop of
                    Nothing -> return False
                    Just people -> return $ length p == 1 && (head p) `elem` (map letter people)

manyPeople :: Validator Application Html String
manyPeople = checkM "Must be all letters that corresponds to people." fn
  where fn p = do peop <- currentPeople
                  case peop of
                    Nothing -> return False
                    Just people -> return $ and (map ((flip elem) (map letter people))  p)

validDate :: Validator Application Html Date
validDate = check "Must be a valid date, like 2011.2.25" $ \(Date y m d) -> and [y>1900,y<2100,m>=1,m<=12,d>=1,d<=31]

positive :: (Ord a, Num a) => Validator Application Html a
positive = check "Must be a positive number." $ \n -> n > 0

addEntryForm :: SnapForm Application Html BlazeFormHtml HouseTabEntry
addEntryForm = mkEntry
    <$> label "By: "     ++> inputText                                      Nothing `validate` onePerson  <++ errors
    <*> label "For: "    ++> inputText                                      Nothing `validate` manyPeople <++ errors
    <*> label "Amount: " ++> inputTextRead "Must be a number, like 10.5."   Nothing `validate` positive   <++ errors
    <*> label "What: "   ++> inputText                                      Nothing `validate` nonEmpty   <++ errors
    <*> label "Date: "   ++> inputTextRead "Must be a date, like 2011.6.30" Nothing `validate` validDate  <++ errors
  where mkEntry b f a wha whe = HouseTabEntry (B8.pack b) (B8.pack wha) whe a (B8.pack f)

addEntry :: Application ()
addEntry = do u <- currentUser
              case u of
               Nothing -> errorP "No User"
               Just user -> do
                 r <- eitherSnapForm addEntryForm "add-entry-form"
                 case r of
                     Left form' -> 
                       heistLocal (bindSplice "formdata" (return $ docContent $ renderHtml $ fst $ renderFormHtml form')) $ render "form"
                     Right entry' -> do
                       let u' = user {houseTabEntries = (houseTabEntries user) ++ [entry']}
                       let u'' = u' {currentResult = run (houseTabPeople u') (houseTabEntries u')}
                       saveAuthUser (authUser u'', additionalUserFields u'')
                       redirect "/entries"
                      

site :: Application ()
site = route [ ("/",            index)
             , ("/entries",     ifTop $ requireUser (newSessionH ()) entriesH)
             , ("/entries/add", addEntry)              
             , ("/people/add",  addPerson)
             , ("/signup",      method GET $ newSignupH)
             , ("/signup",      method POST $ signupH)
             , ("/login",       method GET $ newSessionH ())
             , ("/login",       method POST $ loginHandler "password" Nothing newSessionH redirHome)
             , ("/logout",      method GET $ logoutHandler redirHome)

             ]
       <|> serveDirectory "resources/static"
