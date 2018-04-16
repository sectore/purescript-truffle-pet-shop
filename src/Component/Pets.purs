module Component.Pets
  ( State
  , Query(..)
  , Pets
  , view)
  where

import Prelude

import Component.Pet as P
import Contracts.Adoption as Adoption
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (lift, runExcept)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3 (Address, BlockNumber, ChainCursor(..), _to, defaultTransactionOptions, metamaskProvider, runWeb3)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_gasPrice, eth_getAccounts)
import Network.HTTP.Affjax as AX
import Types (Fx)

type State =
  { loading :: Boolean
  , result :: Either MultipleErrors Pets
  , blockNumber :: Maybe BlockNumber
  , accounts :: Maybe (Array Address)
  , contractAddress :: Maybe Address
  }

type Pets = Array P.Pet

newtype PetSlot = PetSlot P.PetId
derive instance eqPetSlot :: Eq PetSlot
derive instance ordPetSlot :: Ord PetSlot

data Query a
  = Init Address a
  | HandlePetMessage P.PetId P.Message a

type PetsFx = Aff Fx

view :: H.Component HH.HTML Query Unit Void PetsFx
view = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where

  initialState :: State
  initialState =
    { loading: false
    , result: Right []
    , blockNumber: Nothing
    , accounts: Nothing
    , contractAddress: Nothing
    }

  render :: State -> H.ParentHTML Query P.Query PetSlot PetsFx
  render st =
    HH.div
      [ HP.class_ $ ClassName "row" ]
      [ HH.div
          [ HP.class_ $ ClassName "col-xs-12 col-sm-8 col-sm-push-2" ]
          [ HH.h1
            [ HP.class_ $ ClassName "text-center" ]
            [ HH.text "Pet Shop" ]
        , HH.br_
        , HH.br_
        , HH.p_ [ HH.text $ "Contract address: " <> maybe "unknown contract address" show st.contractAddress ]
        , HH.p_ [ HH.text $ "Accounts: " <> maybe "unknown account" show st.accounts ]
        , HH.p_ [ HH.text $ "Block no.: " <> maybe "unknown block" show st.blockNumber ]
        , HH.div_
            [ case st.result of
                Left e ->
                  HH.p_ [ HH.text $ show e ]
                Right pets ->
                  HH.div
                  [ HP.class_ $ ClassName "row" ]
                  (map renderPet pets)
            ]
          ]
      ]

  renderPet :: P.Pet -> H.ParentHTML Query P.Query PetSlot PetsFx
  renderPet p@(P.Pet pet) =
    HH.slot
      (PetSlot pet.id)
      (P.view p)
      unit
    (HE.input (HandlePetMessage pet.id))

  eval :: Query ~> H.ParentDSL State Query P.Query PetSlot Void PetsFx
  eval (Init address next) = do
      H.modify (_ { loading = true, contractAddress = Just address })
      r <- H.liftAff $ AX.get ("./json/pets.json")
      provider <- lift $ liftEff' metamaskProvider
      -- get block number
      bn <- lift $ hush <$> runWeb3 provider eth_blockNumber
      -- get accounts
      acc <- lift $ hush <$> runWeb3 provider eth_getAccounts
      _ <- lift $ log $ "acc: " <> show acc
      gp <- lift $ hush <$> runWeb3 provider eth_gasPrice
      _ <- lift $ log $ "gas price: " <> show gp
      currentState <- H.get
      let txOpts = defaultTransactionOptions # _to .~ currentState.contractAddress
      ad <- lift $ runWeb3 provider $ do
        Adoption.getAdopters txOpts Latest
      _ <- lift $ log $ "adopters " <> show ad
      H.modify (_ { loading = false
                  , result = runExcept $ decodeJSON r.response
                  , accounts = acc
                  , blockNumber = bn
                  })
      pure next
  eval (HandlePetMessage p msg next) = do
    pure next
