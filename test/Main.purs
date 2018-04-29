module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Deploy (deployScript)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner as Spec
import Test.Spec.Runner (run', defaultConfig)

import Test.AdoptionSpec (adoptionSpec)

main :: forall e. Eff
  ( console :: CONSOLE
  , eth :: ETH
  , avar :: AVAR
  , fs :: FS
  , process :: Spec.PROCESS
  , process :: PROCESS
  | e
  ) Unit
main =
  void <<< launchAff $ do
    testConfig <- buildTestConfig "http://localhost:8545" 60 deployScript
    liftEff $ run' defaultConfig {timeout = Just (60 * 1000)} [consoleReporter] do
      adoptionSpec testConfig
