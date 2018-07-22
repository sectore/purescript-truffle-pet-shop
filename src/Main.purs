module Main where

import Prelude

import Component.App as App
import Config (adaptionAddress)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.Aff.Util (awaitBody, runHalogenAff, selectElement) as HA
import Halogen.VDom.Driver (runUI) as HD
import Types (Fx)

main :: Eff Fx Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement $ QuerySelector "#app"
  case el of
    Nothing ->
      throwError $ error "div#app has to be defined"
    Just el' -> do
      io <- HD.runUI App.view unit (fromMaybe body el)
      io.query $ H.action $ App.Init adaptionAddress
