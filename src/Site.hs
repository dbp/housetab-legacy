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

                     
entriesH :: Application ()
entriesH = do u <- currentUser
              case u of
                Nothing -> errorP "No User"
                Just user ->
                  (heistLocal $ (bindSplices [("result", (renderResult $ currentResult user))])) $ render "entries"

{-equalSize :: Validator Application Html WeightedSum
equalSize = check "Lists must be of equal size" $ \(WeightedSum l1 l2) ->
    length l1 == length l2

listForm :: (Read a, Show a) => [a] -> SnapForm Application Html BlazeFormHtml [a]
listForm def = inputTextRead "Can't read list" (Just def) <++ errors-}

personCheck :: Validator Application Html Person
personCheck = check "Shouldnt see this" $ \(Person n l ps) -> True

nonEmpty :: Validator Application Html String
nonEmpty = check "String must not be empty." $ \s -> not $ null s
    
addPersonForm :: SnapForm Application Html BlazeFormHtml Person
addPersonForm = (`validate` personCheck) $ (<++ errors) $ mkPerson
    <$> label "Name: " ++> inputText Nothing `validate` nonEmpty <++ errors
    <*> label "Letter: "  ++> inputText Nothing `validate` nonEmpty <++ errors
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

site :: Application ()
site = route [ ("/",            index)
             , ("/entries",     requireUser (newSessionH ()) entriesH)
             , ("/people/add",  addPerson)
             , ("/signup",      method GET $ newSignupH)
             , ("/signup",      method POST $ signupH)
             , ("/login",       method GET $ newSessionH ())
             , ("/login",       method POST $ loginHandler "password" Nothing newSessionH redirHome)
             , ("/logout",      method GET $ logoutHandler redirHome)

             ]
       <|> serveDirectory "resources/static"
