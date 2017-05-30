{-# language OverloadedStrings #-}
module Web where


import Snap.Core
import Snap.Http.Server

-- import Data.Text
-- import Web.Scotty

import Control.Monad.IO.Class



-- | a REST message is modelled as a record of sender, receiver, HTTP method and request endpoint
data Request i a =
  Request { reqSender :: i
          , reqReceiver :: i
          , reqMethod :: Method  -- GET, POST, etc.
          , request :: a } deriving (Eq, Show)




-- -- * Scotty REST handlers

-- -- | Subscribe a service to the queue
-- handleSubs :: Parsable t => (t -> ActionM ()) -> ScottyM ()
-- handleSubs subsa = 
--   post "/subscribe/:ident" $ do
--     i <- param "ident"
--     subsa i
