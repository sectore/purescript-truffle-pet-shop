module Component.Pets
  ( State
  , Query(..)
  , Pets
  , view)
  where

import Prelude

import Component.Pet as P
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (MultipleErrors)
import Data.Foreign.Generic (decodeJSON)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX

type State =
  { loading :: Boolean
  , result :: Either MultipleErrors Pets
  }

type Pets = Array P.Pet

newtype PetSlot = PetSlot P.PetId
derive instance eqPetSlot :: Eq PetSlot
derive instance ordPetSlot :: Ord PetSlot

data Query a
  = FetchPets a
  | HandlePetMessage P.PetId P.Message a


type Fx eff = Aff (ajax :: AX.AJAX | eff)

view :: forall e. H.Component HH.HTML Query Unit Void (Fx e)
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
    }

  render :: State -> H.ParentHTML Query P.Query PetSlot (Fx e)
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

  renderPet :: P.Pet -> H.ParentHTML Query P.Query PetSlot (Fx e)
  renderPet p@(P.Pet pet) =
    HH.slot
      (PetSlot pet.id)
      (P.view p)
      unit
    (HE.input (HandlePetMessage pet.id))

  eval :: Query ~> H.ParentDSL State Query P.Query PetSlot Void (Fx e)
  eval (FetchPets next) = do
      H.modify (_ { loading = true })
      r <- H.liftAff $ AX.get ("./json/pets.json")
      H.modify (_ { loading = false
                  , result = runExcept $ decodeJSON r.response
                  })
      pure next
  eval (HandlePetMessage p msg next) = do
    pure next
