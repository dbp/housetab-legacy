{-# LANGUAGE OverloadedStrings #-}
module Form where
  
import            Text.Digestive.Types
import            Text.Digestive.Blaze.Html5
import            Text.Digestive.Forms.Snap
import            Text.Digestive.Validate
import            Text.Blaze (Html)
import            Text.XmlHtml (docContent)
import            Text.Blaze.Renderer.XmlHtml (renderHtml)
import qualified  Data.ByteString as BS
import qualified  Data.ByteString.Char8 as B8
import            State
import            Control.Applicative
import            Control.Monad
import            Text.Templating.Heist

import            Application
import            Account

formSplice :: FormHtml Html -> Splice Application
formSplice = return . docContent . renderHtml . fst . renderFormHtml

personCheck :: Validator Application Html Person
personCheck = check "Shouldnt see this" $ \(Person n l ps) -> True

nonEmpty :: Validator Application Html String
nonEmpty = check "String must not be empty." $ \s -> not $ null s
    
lenOne :: Validator Application Html String
lenOne = check "String must be a single character." $ \s -> length s == 1


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

entryForm :: Maybe HouseTabEntry -> SnapForm Application Html BlazeFormHtml HouseTabEntry
entryForm e = mkEntry
    <$> inputHidden (liftM (show.eid) e)
    <*> label "By: "     ++> inputText                                      (lM ewho e)        `validate` onePerson  <++ errors
    <*> label "For: "    ++> inputText                                      (lM ewhopays e)    `validate` manyPeople <++ errors
    <*> label "Amount: " ++> inputTextRead "Must be a number, like 10.5."   (liftM ehowmuch e) `validate` positive   <++ errors
    <*> label "What: "   ++> inputText                                      (lM ewhat e)       `validate` nonEmpty   <++ errors
    <*> label "Date: "   ++> inputTextRead "Must be a date, like 2011.6.30" (liftM ewhen e)    `validate` validDate  <++ errors
  where mkEntry i b f a wha whe = HouseTabEntry (read i) (B8.pack b) (B8.pack wha) whe a (B8.pack f)
        lM f = liftM (B8.unpack . f)

deleteForm :: SnapForm Application Html BlazeFormHtml ()
deleteForm = const ()
    <$> label "Are you sure you want to delete this? " ++> inputHidden Nothing


addPersonForm :: SnapForm Application Html BlazeFormHtml Person
addPersonForm = mkPerson
    <$> label "Name: "    ++> inputText Nothing `validate` nonEmpty <++ errors
    <*> label "Letter: "  ++> inputText Nothing `validate` lenOne <++ errors
  where mkPerson n l = Person (B8.pack n) (head l) []

