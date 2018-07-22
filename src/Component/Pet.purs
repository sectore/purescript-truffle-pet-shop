module Component.Pet
  ( Query(..)
  , Pet(..)
  , PetId
  , Message (..)
  , view)
  where

import Prelude

import Bulma.Columns.Columns (column) as B
import Bulma.Columns.Size (PercentSize(..), isPercentSizeResponsive) as B
import Bulma.Common (Breakpoint(Desktop, Tablet), Color(..), Size(..), unsafeClassName) as B
import Bulma.Components.Card (card, cardContent, cardImage) as B
import Bulma.Elements.Button (Color(..), State(..), Style(..), button, isColor, isState, isStyle) as BB
import Bulma.Elements.Image (Ratio(..), image, isRatio) as B
import Bulma.Elements.Tag (tag) as B
import Bulma.Elements.Title (title) as B
import Bulma.Modifiers.Helpers (Helpers(..), is) as B
import Bulma.Modifiers.Modifiers (isSize, isColor) as B
import Bulma.Modifiers.Typography (Size(..), isSize) as BT
import Control.Monad.Aff (Aff)
import Data.Foreign (readInt, readString)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Index ((!))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Halogen as H
import Halogen.HTML (HTML, br_, figure, li_, text, ul_) as HH
import Halogen.HTML.Elements (button, div, h3, img, strong_) as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenUtil as HU
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
  , loading :: Boolean
  }

type Input = Pet
type State = Pet

derive instance genericPet :: Generic Pet _
derive instance ntPet :: Newtype Pet _

instance showPet :: Show Pet where
  show = genericShow

instance eqPet :: Eq Pet where
  eq = genericEq

instance decodePet :: Decode Pet where
  decode value = do
    id <- value ! "id" >>= readInt
    name <- value ! "name" >>= readString
    picture <- value ! "picture" >>= readString
    age <- value ! "age" >>= readInt
    breed <- value ! "breed" >>= readString
    location <- value ! "location" >>= readString
    pure $ Pet
      { id
      , name
      , picture
      , age
      , breed
      , location
      , adopted: false
      , loading: false
      }

data Query a
  = Adopt a
  | HandleInput Pet a

data Message
  = NotifyAdopt

type PetFx = Aff Fx

view :: H.Component HH.HTML Query Input Message PetFx
view =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render p@(Pet pet) =
    HH.div
      [ HU.classNames
        [ B.column
        , B.isPercentSizeResponsive B.OneThird B.Tablet
        , B.isPercentSizeResponsive B.OneQuarter B.Desktop
        ]
      , HP.id_ $ show pet.id
      ]
      [ HH.div
          [ HU.className B.card ]
          [ HH.div
              [ HU.className B.cardImage ]
              [ HH.figure
                  [ HU.classNames [ B.image, B.isRatio B.OneByOne ]
                  ]
                  [ HH.img
                  [ HP.src pet.picture
                  ]
                ]
              , HH.h3
                  [ HU.classNames $
                      [ B.tag
                      , B.isColor B.Primary
                      , BT.isSize BT.Size6
                      , B.unsafeClassName "tag-adopted"
                      ]
                      <> if not pet.adopted
                        then [B.is B.Invisible]
                        else mempty
                  ]
                  [ HH.text "✓"]
              ]
          , HH.div
            [ HU.className B.cardContent ]
            [ HH.div
              [ HU.className B.cardContent ]
              [ HH.h3
                  [ HU.className B.title ]
                  [ HH.text pet.name ]
              , HH.ul_
                  [ HH.li_
                      [ HH.strong_
                          [ HH.text "Breed "]
                      , HH.text pet.name
                      ]
                  , HH.li_
                      [ HH.strong_
                          [ HH.text "Age "]
                      , HH.text $ show pet.age
                      ]
                  , HH.li_
                      [ HH.strong_
                          [ HH.text "Location"]
                      , HH.br_
                      , HH.text pet.location
                      ]
                  ]
              , HH.button
                  [ HU.classNames
                      [ BB.button
                      , B.isSize B.Large
                      , BB.isColor (BB.CommonColor B.Primary)
                      , B.unsafeClassName "button-adopt"
                      , BB.isStyle $
                          if pet.adopted
                            then BB.Inverted
                            else BB.Outlined
                      , BB.isState $
                          if pet.loading
                            then BB.Loading
                            else BB.Normal
                      ]
                  , HE.onClick (HE.input_ Adopt)
                  , HP.disabled $ pet.adopted
                  ]
                  [ HH.text $
                      if pet.adopted
                        then "Adopted"
                        else "Adopt"
                  ]
              ]
            ]

          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message PetFx
  eval (Adopt next) = do
    H.raise $ NotifyAdopt
    pure next
  eval (HandleInput pet next) = do
    current <- H.get
    when (current /= pet) $ H.put pet
    pure next
