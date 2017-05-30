{-# language OverloadedStrings #-}
module Lib where

import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar
-- import Control.Concurrent.STM.TQueue
import Control.Monad (unless, when, forever)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.IO.Class
import Control.Monad.Free
import Web
-- import Control.Monad.IO.Class

import Control.Monad.Catch



-- * Types



-- newtype Entity i = Entity { entityId :: i } deriving (Eq, Show)

-- | The global state of conductor : a transactional queue (TQueue) of Request s
newtype RequestQueue i a =
  RequestQueue { globalState :: TQueue (Request i a) }

-- | Initial state (empty request queue)
state0 :: STM (RequestQueue i a)
state0 = RequestQueue <$> newTQueue
  

-- | Pop the first element `x` conditionally; if the predicate is True for x, return it in the queue (as last element), otherwise return x for further computation within STM.
readUnless :: TQueue a -> (a -> Bool) -> STM a
readUnless q cond = do
  x <- readTQueue q
  unless (cond x) $ writeTQueue q x
  return x
      











-- * A domain-specific language for interacting with the message queue

data Interaction i next = Subscribe i (i -> next)
                        | ForwardMessage i (i -> next)
                        | Done

instance Functor (Interaction i) where
  fmap f (Subscribe isend fnext) = Subscribe isend (f . fnext)
  fmap f (ForwardMessage isend frec) = ForwardMessage isend (f . frec)
  fmap _ Done = Done








  


-- `liftF` from `free` is the "classy" version : 
-- liftF :: (MonadFree f m, Functor f) => f a -> m a 
liftF_ :: Functor f => f a -> Free f a
liftF_ k = Free (Pure <$> k)



-- | Our interaction DSL, lifted

done :: Free (Interaction i) a
done = liftF_ Done

subscribe :: i -> (i -> a) -> Free (Interaction i) a
subscribe isend frec = liftF_ (Subscribe isend frec)

forwardMessage :: i -> (i -> a) -> Free (Interaction i) a
forwardMessage isend frec = liftF_ (ForwardMessage isend frec)












-- * `Free` allows us to apply distinct interpreters to the same program, e.g. for execution and pretty-printing

-- interpretProgram :: Monad m => Free (Interaction i) a -> m b
interpretProgram :: Monad m => (i -> m i) -> Free (Interaction i) t -> m b
interpretProgram subsf prog = case prog of
  Free (Subscribe isend frec) -> do
    x <- subsf isend
    interpretProgram subsf (frec x)
      -- where
      --   subscribef = undefined


showProgram :: Show i => Free (Interaction i) a -> String
showProgram prog = case prog of
  Free (Subscribe isend frec) ->
    "subscribed " ++ show isend ++ "\n" ++ showProgram (frec isend)
  Free Done -> "done\n"

pprint :: Show i => Free (Interaction i) a -> IO ()
pprint = putStrLn . showProgram






-- * Example programs

test usr = do
  subscribe usr id
  done

-- exampleProgram :: i -> i -> (i -> a) -> Free (Interaction i) a
exampleProgram a b = do
  subscribe a id
  subscribe b id
  done
  -- forwardMessage b
  






-- playground

-- handleRequest :: STM () -> STM () -> ScottyM ()
-- handleRequest geta getb = do
--   get "/" (liftIO $ atomically geta)
--   post "/" (liftIO $ atomically getb)
