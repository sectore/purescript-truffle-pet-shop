module Component.Pets
  ( State
  , Query(..)
  , Pets
  , view)
  where

import Prelude

import Component.Pet (Message(NotifyAdopt), Pet)
import Component.Pet as P
import Contracts.Adoption as Adoption
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (lift, runExcept)
import Data.Array (head, index)
import Data.Either (Either(Left, Right))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((.~), (?~))
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (unwrap, wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Core.BigNumber (embed)
import Network.Ethereum.Web3 (type (:&), Address, BlockNumber, ChainCursor(Latest), D1, D6, DOne, Vector, _from, _gas, _to, defaultTransactionOptions, metamaskProvider, mkAddress, mkHexString, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_blockNumber, eth_gasPrice, eth_getAccounts)
import Network.Ethereum.Web3.Solidity (unVector)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.HTTP.Affjax as AX
import Partial.Unsafe (unsafePartial)
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
        , HH.p_ [ HH.text $ "Accounts: " <> maybe "unknown accounts" show st.accounts ]
        , HH.p_ [ HH.text $ "Block no.: " <> maybe "unknown block no." show st.blockNumber ]
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
  eval (Init adress next) = do
      H.modify (_ { loading = true
                  , contractAddress = Just adress
                  })
      result <- H.liftAff $ AX.get ("./json/pets.json")
      provider <- lift $ liftEff' metamaskProvider
      -- get block number
      bn <- lift $ hush <$> runWeb3 provider eth_blockNumber
      -- get accounts
      acc <- lift $ hush <$> runWeb3 provider eth_getAccounts
      _ <- lift $ log $ "accounts: " <> show acc
      -- get gas price
      gp <- lift $ hush <$> runWeb3 provider eth_gasPrice
      _ <- lift $ log $ "gas price: " <> show gp
      currentState <- H.get
      let txOpts = defaultTransactionOptions
                      # _to .~ currentState.contractAddress
      -- get adopters
      adopters <- lift $ runWeb3 provider $
        Adoption.getAdopters txOpts Latest
      adopters' <- lift $ runWeb3 provider $ Adoption.adopters txOpts Latest (unsafePartial $ fromJust $ uIntNFromBigNumber s256 $ embed 15)
      _ <- lift $ log $ "adopters' " <> show adopters'
      H.modify (_ { loading = false
                  , accounts = acc
                  , blockNumber = bn
                  })
      let (response :: Either MultipleErrors Pets) = runExcept $ decodeJSON result.response
      case response of
          Right r -> do
              case adopters of
                Right a -> do
                  let updatedAdopters = setAdopted (Just r) (hush a)
                  lift $ log $ "adopters " <> show a
                  -- lift $ log $ "updatedAdopters " <> show updatedAdopters
                  H.modify (_ { result = updatedAdopters } )
                Left _ ->
                  lift $ log "error adopters"
          Left _ ->
              lift $ log "error response"

      pure next

  eval (HandlePetMessage p msg next) = do
    case msg of
      NotifyAdopt pId-> do
        provider <- lift $ liftEff' metamaskProvider
        -- get accounts
        accounts <- lift $ hush <$> runWeb3 provider eth_getAccounts
        _ <- case accounts of
              Just accs -> do
                lift $ log $ "accounts " <> show accounts
                let mAccount = head accs
                -- lift $ log $ "account " <> mAccount
                case mAccount of
                  Just account -> do
                    -- uIntNFromBigNumber :: forall n . KnownSize n => DLProxy n -> BigNumber -> Maybe (UIntN n)
                    lift $ log $ "pId " <> show pId
                    let bPetId = embed pId
                    let mUPetId = uIntNFromBigNumber s256 bPetId
                    case mUPetId of
                      Just uPetId -> do
                        cState <- H.get
                        lift $ log $ "from " <> show account
                        lift $ log $ "to " <> show cState.contractAddress
                        lift $ log $ "uIntNFromBigNumber " <> show uPetId
                        let txOpts = defaultTransactionOptions
                                            # _from .~ Just account
                                            # _to .~ cState.contractAddress
                                            -- # _gas .~ parseBigNumber hexadecimal "0x2dc2dc"
                                            # _gas ?~ embed 4712388
                                            -- # _gasPrice ?~ embed 2147483647
                        lift $ log $ "txOpts " <> show txOpts
                        tx <- lift $ runWeb3 provider $ Adoption.adopt txOpts {petId: uPetId}
                        -- let filterAdopted = eventFilter (Proxy :: Proxy Adoption.Adopted) $ unsafePartial $ fromJust cState.contractAddress
                        -- _ <- liftAff $ runWeb3 provider $
                        --                   event filterAdopted $ \e@(Adoption.Adopted cs) -> do
                        --                     liftAff $ log $ "Received Adopted event: " <> show e
                        --                     pure TerminateEvent
                        -- lift $ log $ "send adopt tx " <> show tx
                        lift $ log $ "adopt tx: " <> show tx
                      Nothing -> do
                        lift $ log "error creating uIntNFromBigNumber..."
                  Nothing ->
                    lift $ log "error no first account"
              Nothing ->
                lift $ log $ "no accounts "
        lift $ log $ "handle NotifyAdopt "
    pure next

-- Helper to update adopted status of a Pet comparing to contracts
setAdopted :: Maybe Pets -> Maybe (Vector (D1 :& DOne D6) Address) -> Maybe Pets
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
