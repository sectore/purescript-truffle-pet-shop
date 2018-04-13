module Main where

import Prelude

import Component.Pets (Query(..), view) as Pets
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.Aff (HalogenEffects) as HA
import Halogen.Aff.Util (awaitBody, runHalogenAff, selectElement) as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)

main :: Eff (HA.HalogenEffects (ajax :: AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement $ QuerySelector "#app"
  case el of
    Nothing ->
      throwError $ error "#app has to be defined"
    Just el' -> do
      io <- runUI Pets.view unit (fromMaybe body el)
      io.query $ H.action Pets.FetchPets
