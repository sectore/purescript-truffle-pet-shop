module Component.Pet
  ( Query(..)
  , Pet(..)
  , PetId
  , Message
  , view)
  where

import Prelude

import Data.Bifunctor (bimap)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML (HTML, text) as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (br_, button, div, h3, img, span, strong_) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type PetId = Int

newtype Pet = Pet
  { id :: PetId
  , name :: String
  , picture :: String
  , age :: Int
  , breed :: String
  , location :: String
  }

derive instance genericPet :: Generic Pet _

instance showPet :: Show Pet where
  show = genericShow

instance decodePet :: Decode Pet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodePet :: Encode Pet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

data Query a
  = Adopt a

data Message
  = NotifyAdopt

view :: forall m. Pet -> H.Component HH.HTML Query Unit Message m
view p =
  H.component
    { initialState: const p
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: Pet -> H.ComponentHTML Query
  render (Pet pet) =
    bimap id id $
      HH.div
        [ HP.class_ $ ClassName "col-sm-6 col-md-4 col-lg-3" ]
        [ HH.div
            [ HP.class_ $ ClassName "panel panel-default panel-pet" ]
            [ HH.div
              [ HP.class_ $ ClassName "panel-heading" ]
              [ HH.h3
                [ HP.class_ $ ClassName "panel-title" ]
                [ HH.text pet.name ]
              ]
            , HH.div
              [ HP.class_ $ ClassName "panel-body" ]
              [ HH.img
                  [ HP.alt "140x140"
                  , HP.class_ $ ClassName "img-rounded img-center"
                  , HP.src pet.picture
                  -- , HP.prop (PropName "style") "width: 100%"
                  ]
              , HH.br_
              , HH.br_
              , HH.strong_ [ HH.text "Breed"]
              , HH.text ": "
              , HH.span
                  [ HP.class_ $ ClassName "pet-breed" ]
                  [ HH.text pet.name ]
              , HH.br_
              , HH.strong_ [ HH.text "Age"]
              , HH.text ": "
              , HH.span
                  [ HP.class_ $ ClassName "pet-age" ]
                  [ HH.text $ show pet.age ]
              , HH.br_
              , HH.strong_ [ HH.text "Location"]
              , HH.text ": "
              , HH.span
                  [ HP.class_ $ ClassName "pet-location" ]
                  [ HH.text pet.location ]
              , HH.br_
              , HH.br_
              , HH.button
                  [ HP.class_ $ ClassName "btn btn-default btn-adopt"
                  , HE.onClick (HE.input_ Adopt)
                  ]
                  [ HH.text "Adopt" ]
              ]
            ]
          ]

  eval :: Query ~> H.ComponentDSL Pet Query Message m
  eval (Adopt next) = do
    H.raise NotifyAdopt
    pure next
