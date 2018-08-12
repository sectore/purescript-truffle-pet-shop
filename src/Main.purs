module Main where

import Prelude

import Component.App as App
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Environment (mkEnv)
import Halogen as H
import Halogen.Aff.Util (awaitBody, runHalogenAff, selectElement) as HA
import Halogen.VDom.Driver (runUI) as HD
import Types (Fx, runPetShopM)

main :: Eff Fx Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement $ QuerySelector "#app"
  case el of
    Nothing ->
      throwError $ error "div#app has to be defined"
    Just el' -> do
      let app = H.hoist (runPetShopM mkEnv) App.view
      HD.runUI app unit (fromMaybe body el)
