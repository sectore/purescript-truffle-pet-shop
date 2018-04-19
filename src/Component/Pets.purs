module Component.Pets
  ( State
  , Query(..)
  , Pets
  , view)
  where

import Prelude

import Component.Pet (Pet)
import Component.Pet as P
import Contracts.Adoption as Adoption
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (lift, runExcept)
import Data.Array (index)
import Data.Either (Either(Left, Right))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((.~))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Web3 (Address, BlockNumber, ChainCursor(..), Vector, _to, defaultTransactionOptions, metamaskProvider, mkAddress, mkHexString, runWeb3)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_gasPrice, eth_getAccounts)
import Network.Ethereum.Web3.Solidity (unVector)
import Network.Ethereum.Web3.Solidity.Size (N16)
import Network.HTTP.Affjax as AX
import Types (Fx)

type State =
  { loading :: Boolean
  , result :: Maybe Pets
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
    , result: Nothing
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
                Nothing ->
                  HH.p_ [ HH.text "" ]
                Just pets ->
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
      -- get gas price
      gp <- lift $ hush <$> runWeb3 provider eth_gasPrice
      _ <- lift $ log $ "gas price: " <> show gp
      currentState <- H.get
      let txOpts = defaultTransactionOptions # _to .~ currentState.contractAddress
      -- get adopters
      adopters <- lift $ runWeb3 provider $
        Adoption.getAdopters txOpts Latest
      H.modify (_ { loading = false
                  , accounts = acc
                  , blockNumber = bn
                  })
      let (response :: Either MultipleErrors Pets) = runExcept $ decodeJSON r.response
      case response of
          Right r -> do
              case adopters of
                Right a -> do
                  cState <- H.get
                  let updatedAdopters = setAdopted (Just r) (hush a)
                  lift $ log $ "a " <> show a
                  lift $ log $ "updatedAdopters " <> show updatedAdopters
                  H.modify (_ { result = updatedAdopters } )
                Left _ ->
                  lift $ log "error adopters"
          Left _ ->
              lift $ log "error response"

      pure next
  eval (HandlePetMessage p msg next) = do
    pure next

-- Helper to update adopted status of a Pet comparing to contracts
setAdopted :: Maybe Pets -> Maybe (Vector N16 Address) -> Maybe Pets
setAdopted mPets mAddresses = do
  pets <- mPets
  addresses <- unVector <$> mAddresses
  mEmptyContract <- mkAddress <$> mkHexString "0x0000000000000000000000000000000000000000"
  case mEmptyContract of
    Nothing -> Nothing
    Just emptyContract ->
      -- loop all pets to compare + update its adopted status
      pure $ mapWithIndex
        (\i (pet :: Pet) -> do
          let mAddr = index addresses i
          let isAdopted = maybe false ((/=) emptyContract) mAddr
          wrap $ _{ adopted = isAdopted } $ unwrap pet)
        pets
