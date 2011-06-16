{-# LANGUAGE OverloadedStrings #-}

module Views.Result where
  
  
import            Text.Templating.Heist
import qualified  Data.Text.Encoding as TE
import qualified  Data.Text as T

import            Models.Result
import            Models.Person


renderPersonResult :: Monad m => (Person,Spent,Owes) -> Splice m
renderPersonResult (person,spent,owes) = do
  runChildrenWithText [("personName",   TE.decodeUtf8 $ name person)
                      ,("personLetter", T.pack $ [letter person])
                      ,("personSpent",  T.pack $ show spent)
                      ,("personOwes",   T.pack $ show owes)]
                       
renderResult :: Monad m => Result -> Splice m
renderResult (Result people date) = mapSplices renderPersonResult people