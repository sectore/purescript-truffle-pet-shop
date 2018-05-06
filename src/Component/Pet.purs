module Component.Pet
  ( Query(..)
  , Pet(..)
  , PetId
  , Message (..)
  , view)
  where

import Prelude

import Bulma.Common (Breakpoint(..), Color(Primary), Is(..), runClassName, runClassNames) as B
import Bulma.Components.Card (card, cardContent, cardImage) as B
import Bulma.Elements.Button (Color(..), button, isColor) as BB
import Bulma.Elements.Elements (content) as B
import Bulma.Elements.Image (isRatio, Ratio(..)) as B
import Bulma.Columns.Columns (column) as B
import Bulma.Columns.Size (PercentSize(..), isPercentSize, isPercentSizeResponsive, isSize) as B
import Bulma.Elements.Title (title) as B
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Data.Bifunctor (bimap)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Halogen (lift)
import Halogen as H
import Halogen.HTML (HTML, span_, text) as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Elements (br_, button, div, h3, img, span, strong_) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Fx)

type PetId = Int

newtype Pet = Pet
  { id :: PetId
  , name :: String
  , picture :: String
  , age :: Int
  , breed :: String
  , location :: String
  , adopted :: Boolean
  }

derive instance genericPet :: Generic Pet _
derive instance ntPet :: Newtype Pet _

instance showPet :: Show Pet where
  show = genericShow

instance eqPet :: Eq Pet where
  eq = genericEq

instance decodePet :: Decode Pet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodePet :: Encode Pet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

data Query a
  = Adopt a
  | HandleInput Pet a

data Message
  = NotifyAdopt PetId

type PetFx = Aff Fx

view :: H.Component HH.HTML Query Pet Message PetFx
view =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: Pet -> H.ComponentHTML Query
  render p@(Pet pet) =
    bimap id id $
      HH.div
        [ HP.class_ $ ClassName $ B.runClassNames
          [ B.column
          , B.isPercentSizeResponsive B.Half B.Tablet
          , B.isPercentSizeResponsive B.OneThird B.Desktop
          ]
        ]
        [ HH.div
            [ HP.class_ $ ClassName $ B.runClassNames [ B.card ] ]
            [ HH.img
                [ HP.alt "140x140"
                , HP.class_ $ ClassName $ B.runClassNames
                    [ B.cardImage
                    , B.isRatio B.Square
                    ]
                , HP.src pet.picture
                ]
            , HH.div
              [ HP.class_ $ ClassName $ B.runClassName B.cardContent ]
              [ HH.div
                [ HP.class_ $ ClassName $ B.runClassName B.content ]
                [ HH.h3
                    [ HP.class_ $ ClassName $ B.runClassName B.title ]
                    [ HH.text pet.name ]
                , HH.div
                  [ HP.class_ $ ClassName "panel-body" ]
                  [ HH.br_
                  , HH.br_
                  , HH.strong_ [ HH.text "Breed"]
                  , HH.text ": "
                  , HH.span
                      [ HP.class_ $ ClassName "pet-breed" ]
                      [ HH.text pet.name ]
                  , HH.br_
                  , HH.strong_ [ HH.text "id"]
                  , HH.text ": "
                  , HH.span
                      [ HP.class_ $ ClassName "" ]
                      [ HH.text $ show pet.id ]
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
                  , HH.text "adopted: "
                  , HH.span_
                      [ HH.text $ show pet.adopted ]
                  , HH.br_
                  , HH.br_
                  , HH.text "pet data: "
                  , HH.span_
                      [ HH.text $ show p ]
                  , HH.br_
                  , HH.br_
                  , HH.button
                      [ HP.class_ $ ClassName $ B.runClassNames $
                          [ BB.button
                          , BB.isColor (BB.CommonColor B.Primary)
                          ]
                      , HE.onClick (HE.input_ Adopt)
                      , HP.disabled $ pet.adopted
                      ]
                      [ HH.text "Adopt" ]
                  ]
                ]

              ]
            ]
          ]

  eval :: Query ~> H.ComponentDSL Pet Query Message PetFx
  eval (Adopt next) = do
    pet <- H.get
    _ <- lift <<< log $ "pet to adopt: " <> show pet
    H.raise <<< NotifyAdopt <<< _.id $ unwrap pet
    pure next

  eval (HandleInput pet next) = do
    current <- H.get
    when (current /= pet) $ H.put pet
    pure next
