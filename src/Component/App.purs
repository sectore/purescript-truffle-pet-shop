module Component.App where

import Prelude

import Bulma.Layout.Layout (container) as B
import Component.Footer as Footer
import Component.Header as Header
import Component.Notification as N
import Component.Pet (Pet(..))
import Component.Pet as P
import Component.PetList as PL
import Contract.Adoption as Adoption
import Control.Error.Util (hush)
import Control.Monad.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (class MonadState)
import Data.Array (head, index)
import Data.Either (Either(Left, Right))
import Data.Either.Nested (Either2)
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((.~), (?~))
import Data.Maybe (Maybe(..), fromJust, isNothing, maybe)
import Data.Newtype (over)
import Environment (Env)
import Halogen (liftEff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import HalogenUtil (emptyView)
import HalogenUtil as HU
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Core.Signatures (Address, mkAddress)
import Network.Ethereum.Web3 (CallError, ChainCursor(Latest), EventAction(ContinueEvent), Provider, Vector, Web3Error, _from, _to, defaultTransactionOptions, embed, event, eventFilter, metamaskProvider, mkHexString, runWeb3, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Solidity (unVector)
import Network.Ethereum.Web3.Solidity.Sizes (S16, s256)
import Network.HTTP.Affjax as Ajax
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))
import Types (Fx)



data PetListSlot = PetListSlot
derive instance eqSlot :: Eq PetListSlot
derive instance ordSlot :: Ord PetListSlot

notificationPath ::  CP.ChildPath N.Query ChildQuery N.Slot ChildSlot
notificationPath = CP.cp1

petListPath :: CP.ChildPath PL.Query ChildQuery PetListSlot ChildSlot
petListPath = CP.cp2

type ChildSlot = Either2 N.Slot PetListSlot
type ChildQuery = Coproduct2 N.Query PL.Query

type Pets = Array P.Pet

data PetsError
  = PetsWeb3Error Web3Error
  | PetsCallError CallError
  | PetsJsonError MultipleErrors

type State =
  { pets :: Maybe (Either PetsError Pets)
  , contractAddress :: Maybe Address
  , primaryAccount :: Maybe Address
  , provider :: Maybe Provider
  , mErrors :: Maybe (Array String)
  , mWarnings :: Maybe (Array String)
  , mInfos :: Maybe (Array String)
  }

initialState :: State
initialState =
  { pets: Nothing
  , contractAddress: Nothing
  , primaryAccount: Nothing
  , provider: Nothing
  , mErrors: Nothing
  , mWarnings: Nothing
  , mInfos: Nothing
  }

data Query a
  = Init a
  | Adopted Adoption.Adopted a
  | HandlePetListMessage PL.Message a
  | HandleNotificationMessage N.NotificationLevel N.Message a

type AppFx = Aff Fx


view
  :: forall m
   . MonadAff Fx m
  => MonadAsk Env m
  => H.Component HH.HTML Query Unit Void m
view = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }
  where
    render
      :: State
      -> H.ParentHTML Query ChildQuery ChildSlot m
    render { mErrors, mWarnings, mInfos, pets } =
      HH.div
        [ HU.className B.container ]
        [ maybeNotificationView mErrors N.Error
        , maybeNotificationView mWarnings N.Warning
        , maybeNotificationView mInfos N.Success
        , Header.view
        , case pets of
            Nothing -> HH.text ""
            Just (Left err) -> emptyView
            Just (Right pets') ->
              HH.slot'
                petListPath
                PetListSlot
                PL.view
                pets'
                (HE.input HandlePetListMessage)
        , Footer.view
        ]

    eval
      :: Query
      ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval = case _ of
      (Init  next) -> do
        {contractAddress} <- ask
        -- initial data are loaded from json
        result <- H.liftAff $ Ajax.get ("./json/pets.json")
        let (response :: Either MultipleErrors Pets) = runExcept $ decodeJSON result.response
        case response of
          Right petsFromJSON -> do
            provider <- H.liftEff $ try metamaskProvider
            case provider of
              Right provider' -> do
                mPrimaryAccount <- H.liftAff $ maybe Nothing (head) <<< hush <$> runWeb3 provider' eth_getAccounts
                when (isNothing mPrimaryAccount) $ do
                    errMsg <- updateErrorMsgFromState "Primary account not found. Make sure that you have logged in into MetaMask."
                    H.modify (_ { mErrors = errMsg
                                })
                H.modify (_ { primaryAccount = mPrimaryAccount, contractAddress = Just contractAddress } )

                let txOpts = defaultTransactionOptions
                            # _to ?~ contractAddress
                -- adopters
                adopters <- H.liftAff $ runWeb3 provider' $ Adoption.getAdopters txOpts Latest
                case adopters of
                  -- Result
                  Right (Right adopters') -> do
                    let pets' = setAdoptedByContract petsFromJSON adopters'
                    H.modify (_ { pets = Just $ Right pets' } )
                    -- subscribe `Adopted` event
                    H.subscribe $ ES.eventSource' (\emit -> do
                        let filterAdopted = eventFilter (Proxy :: Proxy Adoption.Adopted) contractAddress
                        fiber <- launchAff $ runWeb3 provider' $ event filterAdopted $
                                    \event -> do
                                      -- liftAff $ C.log $ "Received Adopted event: " <> show event
                                      liftEff $ emit event
                                      pure ContinueEvent
                        pure $ launchAff_ $ killFiber (error "cleanup") fiber
                      )
                      (Just <<< flip Adopted ES.Listening)

                  -- CallError
                  Right (Left callError) -> do
                    errMsg <- updateErrorMsgFromState "Loading addresses of adopters failed. Make sure that you have all smart contracts deployed before."
                    H.modify (_ { mErrors = errMsg
                                , pets = Just $ Left $ PetsCallError callError
                                })
                  -- Web3Error
                  Left web3Error -> do
                    errMsg <- updateErrorMsgFromState "Loading addresses of adopters failed. Make sure that you have Metamask enabled and you are logged in."
                    H.modify (_ { mErrors = errMsg
                                , pets = Just $ Left $ PetsWeb3Error web3Error
                                })
              Left error -> do
                errMsg <- updateErrorMsgFromState "No Metamask provider found. Get Metamask at https://metamask.io/"
                H.modify (_ { mErrors = errMsg })
          -- MultipleErrors
          Left foreignErrors -> do
            errMsg <- updateErrorMsgFromState "Parsing of `pets.json` failed"
            H.modify (_ { mErrors = errMsg
                        , pets = Just $ Left $ PetsJsonError foreignErrors
                        })
        pure next

      Adopted (Adoption.Adopted {petId}) next -> do
        {pets} <- H.get
        case pets of
          Just (Right pets') -> do
            let petId' = unsafeToInt $ unUIntN petId
            H.modify _ { pets = Just $ Right $ setLoadedById petId' false $ setAdoptedById petId' pets'
                        }
          _ ->
            pure unit
        pure next

      HandlePetListMessage msg next -> do
        case msg of
          PL.NotifyAdoptPet petId -> do
            {contractAddress, primaryAccount, pets} <- H.get
            case pets of
              Just (Right pets') -> do
                provider <- H.lift $ H.liftEff metamaskProvider
                let bPetId = embed petId
                let uPetId = unsafePartial $ fromJust $ uIntNFromBigNumber s256 bPetId
                -- -- Set loading state while adopting a pet
                H.modify _ { pets = Just $ Right $ setLoadedById petId true pets' }
                let txOpts = defaultTransactionOptions
                            # _from .~ primaryAccount
                            # _to .~ contractAddress
                tx <- H.liftAff $ runWeb3 provider $ Adoption.adopt txOpts {petId: uPetId}
                pure unit
                case tx of
                  Right tx' ->
                    H.modify _ { mInfos = Just ["Sending adopt tx succeeded: " <> show tx']
                              }
                  Left err -> do
                    errMsg <- updateErrorMsgFromState $ "Sending adopt tx failed: " <> show err
                    H.modify _ { mErrors = errMsg
                              , pets = Just $ Right $ setLoadedById petId false pets'
                              }
              _ ->
                pure unit

        pure next

      HandleNotificationMessage level msg next -> do
        case msg of
          N.NotififyClose ->
            let newState = case level of
                      N.Warning -> _ { mWarnings = Nothing }
                      N.Error ->  _ { mErrors = Nothing }
                      N.Success ->  _ { mInfos = Nothing }
            in
              H.modify newState
        pure next

updateErrorMsgFromState
  :: forall m
  . MonadState State m
  => String
  -> m (Maybe (Array String))
updateErrorMsgFromState msg = do
  {mErrors} <- H.get
  pure $ mErrors <> Just [msg]

maybeNotificationView
  :: forall env m
   . MonadAff Fx m
  => MonadAsk env m
  => Maybe (Array String)
  -> N.NotificationLevel
  -> H.ParentHTML Query ChildQuery ChildSlot m
maybeNotificationView mMsgs level =
  case mMsgs of
  Nothing -> emptyView
  Just msgs ->
    HH.slot'
      notificationPath
      (N.NotificationSlot level)
      N.view
      (N.mkNotification level msgs)
      (HE.input $ HandleNotificationMessage level)

---------------------------------------------
-- Helpers
---------------------------------------------

setAdoptedByContract :: Pets -> Vector S16 Address -> Pets
setAdoptedByContract pets vAddresses =
  let arrAddresses = unVector vAddresses
      (emptyContract :: Address) = unsafePartial $ fromJust $ mkAddress =<< mkHexString "0x0000000000000000000000000000000000000000"
  in mapWithIndex
      (\i ((Pet pet) :: P.Pet) ->
        let mAddr = index arrAddresses i
            isAdopted = maybe false ((/=) emptyContract) mAddr
        in Pet $ pet{ adopted = isAdopted }
      )
      pets

setAdoptedById :: P.PetId -> Pets -> Pets
setAdoptedById pId = do
  map setAdopted
  where
    setAdopted pet@(Pet{id, adopted}) =
      if not adopted then
        over Pet (_{ adopted = id == pId, loading = false }) pet
      else pet

setLoadedById :: P.PetId -> Boolean -> Pets -> Pets
setLoadedById pId loading = do
  map setLoading
  where
    setLoading pet@(Pet{id}) =
      if id == pId then
        over Pet (_{ loading = loading }) pet
      else pet
