{-# LANGUAGE FlexibleContexts #-}

-- Types and functions for working efficiently with JSON data.


module Yesod.Form.JSON 

(
-- * The example of use
-- $use

-- * Functions

    runJSONForm
,   jsonField
) 

where

import Prelude
import Data.Aeson (eitherDecode, encode)
import Yesod.Form.Types
import Data.Text (Text, pack)
import Control.Applicative (Applicative (..))
import Yesod.Core
import Control.Monad (liftM)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Control.Arrow ((***))
import Yesod.Form.Input (FormInput(..))
import qualified Data.HashMap.Strict as HM (toList)
import Yesod.Form.Functions (parseHelper)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as B (fromStrict, toStrict)


-- |Run JSON form
runJSONForm ::  MonadHandler m => FormInput m a -> m a
runJSONForm (FormInput f) = do
    obj <- requireJsonBody
    let env  = toEnv obj
    m <- getYesod
    l <- languages
    emx <- f m l env Map.empty
    case emx of
        Left errs -> invalidArgs $ errs []
        Right x -> return x


-- |Obtain JSON field from json request
jsonField :: (Monad m, FromJSON a) => RenderMessage (HandlerSite m) FormMessage => Field m a
jsonField = Field (parseHelper helper) undefined undefined
  where
    helper json = case (eitherDecode . B.fromStrict . encodeUtf8) json of
                Left err -> Left $ MsgInvalidEntry $ pack err
                Right v  -> Right v

toEnv :: Value -> Env
toEnv (Object obj) = 
    let l = map json2Text $ HM.toList obj
    in Map.fromList $ catMaybes l
    
  where  
  
    json2Text (name, obj@(Object _)) = Just $ (name, [(decodeUtf8 . B.toStrict . encode) obj])  
    json2Text (name, arr@(Array _))  = Just $ (name, [(decodeUtf8 . B.toStrict . encode) arr])  
    json2Text (name, String str)     = Just $ (name, [str])
    json2Text (name, Number n)       = Just $ (name, [pack $ show n])
    json2Text (name, Bool b)         = Just $ (name, [pack $ show b])
    
toEnv _ = Map.empty

-- toMap :: [(Text, a)] -> Map.Map Text [a]
-- toMap = Map.unionsWith (++) . map (\(x, y) -> Map.singleton x [y])


-- $use
--  
--> data Offer = Offer {
-->      name :: Text
-->    , description Text
-->    , supplier :: SupplierId
-->    , category :: CategoryId
-->    , tags :: [TagId] 
-->    , images :: [Image]
-->    , variants :: [Variant]
-->    , active :: Bool
--> }
--
-- > offer <- runJSONForm $ Offer <$> ireq textField "name"
-- >                              <*> ireq textField "description"
-- >                              <*> ireq jsonField "supplier" 
-- >                              <*> ireq jsonField "category")
-- >                              <*> ireq jsonField "tags")
-- >                              <*> ireq jsonField "images"
-- >                              <*> (fmap nub $ ireq jsonField "variants")
-- >                              <*> ireq boolField "active"     

