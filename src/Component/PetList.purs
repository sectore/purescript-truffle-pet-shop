module Component.PetList where

import Prelude

import Bulma.Columns.Columns (columns, isMultiline) as B
import Bulma.Layout.Layout (section) as B
import Component.Pet as P
import Control.Monad.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import HalogenUtil as HU
import Network.Ethereum.Core.Signatures (Address)
import Types (Fx)

type PetsFx = Aff Fx

type Pets = Array P.Pet
type Input = Pets
type State = Pets

newtype PetSlot = PetSlot P.PetId
derive instance eqSlot :: Eq PetSlot
derive instance ordSlot :: Ord PetSlot

data Query a
  = Init Address a
  | HandlePetMessage P.PetId P.Message a
  | HandleInput Input a

data Message
  = NotifyAdoptPet P.PetId

view :: H.Component HH.HTML Query Input Message PetsFx
view = H.parentComponent
  { initialState: id
  , render
  , eval
  , receiver: HE.input HandleInput
  }
  where
    render :: State -> H.ParentHTML Query P.Query PetSlot PetsFx
    render pets =
      HH.section
        [ HU.className B.section ]
        [ HH.div
            [ HU.classNames
                [ B.columns
                , B.isMultiline
                ]
            ]
            [ HH.div
                [ HU.classNames
                    [ B.columns
                    , B.isMultiline
                    ]
                ]
                $ map renderPet pets
            ]
        ]

    renderPet :: P.Pet -> H.ParentHTML Query P.Query PetSlot PetsFx
    renderPet p@(P.Pet pet) =
      HH.slot
        (PetSlot pet.id)
        P.view
        p
      (HE.input (HandlePetMessage pet.id))

    eval :: Query ~> H.ParentDSL State Query P.Query PetSlot Message PetsFx
    eval (Init _ next) = do
      pure next
    eval (HandlePetMessage pId msg next) = do
      case msg of
        P.NotifyAdopt ->
          H.raise $ NotifyAdoptPet pId
      pure next
    eval (HandleInput updatedPets next) = do
      pets <- H.get
      when (pets /= updatedPets) $ H.put updatedPets
      pure next
