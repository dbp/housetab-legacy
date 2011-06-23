{-# LANGUAGE OverloadedStrings #-}

module Views.Result where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T
import            Data.Maybe (fromMaybe)
import            Snap.Extension.DB.MongoDB (bs2objid, objid2bs)

import            Models.Result
import            Models.Person

import            Views.Site

renderPersonResult :: Monad m => (Person,Spent,Owes) -> Splice m
renderPersonResult (person,spent,owes) = do
  runChildrenWithText [("personId",     TE.decodeUtf8 $ fromMaybe "" $ fmap objid2bs $ pId person)
                      ,("personName",   TE.decodeUtf8 $ pName person)
                      ,("personSpent",  T.pack $ moneyShow spent)
                      ,("personOwes",   T.pack $ moneyShow (negate owes))]
                       
renderResult :: Monad m => Result -> Splice m
renderResult (Result people date) = mapSplices renderPersonResult people