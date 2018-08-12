module Types where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, asks, runReaderT)
import DOM (DOM)
import Data.Newtype (class Newtype)
import Environment (Env)
import Network.Ethereum.Web3 (ETH)
import Network.HTTP.Affjax (AJAX)
import Type.Equality as TE

type Fx =
  ( avar :: AVAR
  , ref :: REF
  , dom :: DOM
  , ajax :: AJAX
  , console :: CONSOLE
  , eth :: ETH
  , exception :: EXCEPTION
  )

newtype PetShopM a = PetShopM (ReaderT Env (Aff Fx) a)
derive instance ntPetShopM :: Newtype (PetShopM a) _
derive newtype instance funcPetShopM :: Functor PetShopM
derive newtype instance appPetShopM :: Applicative PetShopM
derive newtype instance applyPetShopM :: Apply PetShopM
derive newtype instance bindPetShopM :: Bind PetShopM
derive newtype instance mPetShopM :: Monad PetShopM
derive newtype instance mEffPetShopM :: MonadEff (ajax :: AJAX, avar :: AVAR, console :: CONSOLE, dom :: DOM , eth :: ETH , exception :: EXCEPTION , ref :: REF) PetShopM
derive newtype instance mAffPetShopM :: MonadAff (ajax :: AJAX, avar :: AVAR, console :: CONSOLE, dom :: DOM , eth :: ETH , exception :: EXCEPTION , ref :: REF) PetShopM
instance mAskPetShopM :: TE.TypeEquals env Env => MonadAsk env PetShopM where
  ask = PetShopM $ asks TE.from

runPetShopM :: forall a. Env -> PetShopM a -> (Aff Fx) a
runPetShopM env (PetShopM m) = runReaderT m env
