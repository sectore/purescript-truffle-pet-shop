module Test.AdoptionSpec
  ( adoptionSpec
  )
  where

import Prelude

import Chanterelle.Test (TestConfig)
import Contracts.Adoption as Adoption
import Control.Monad.Aff.AVar (AVAR, makeEmptyVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Data.Array ((!!))
import Data.Either (fromRight)
import Data.Lens.Setter ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Network.Ethereum.Web3 (Address, ETH, EventAction(TerminateEvent), _from, _to, defaultTransactionOptions, embed, event, eventFilter, runWeb3, uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Prelude (Proxy(..))

adoptionSpec
  :: forall r eff.
     TestConfig (adoptionAddr :: Address | r)
  -> Spec (avar :: AVAR, eth :: ETH, console :: CONSOLE |eff) Unit
adoptionSpec {provider, accounts, adoptionAddr} =
  describe "Adoption Contract" do

    it "adopt first pet" $ do
      let primaryAccount = unsafePartial fromJust $ accounts !! 0
      petIdVar <- makeEmptyVar
      bn <- unsafePartial fromRight <$> runWeb3 provider eth_blockNumber
      let pId = unsafePartial $ fromJust <<< uIntNFromBigNumber s256 $ embed 0
          txOptions = defaultTransactionOptions # _from .~ Just primaryAccount
                                                # _to .~ Just adoptionAddr
      hx <- runWeb3 provider $ Adoption.adopt txOptions {petId: pId}
      liftEff <<< log $ "adopt tx hash: " <> show hx

      let filterAdopted = eventFilter (Proxy :: Proxy Adoption.Adopted) adoptionAddr
      _ <- liftAff $ runWeb3 provider $
        event filterAdopted $ \ev@(Adoption.Adopted event) -> do
          liftEff $ log $ "Received Event: " <> show ev
          _ <- liftAff $ putVar event.petId petIdVar
          pure TerminateEvent
      val <- takeVar petIdVar
      val `shouldEqual` pId
