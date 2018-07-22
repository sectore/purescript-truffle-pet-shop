module Component.Notification where


import Prelude

import Bulma.Common (Color(Danger, Warning, Success)) as B
import Bulma.Elements.Elements (delete, notification) as B
import Bulma.Modifiers.Modifiers as BModifier
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import HalogenUtil as HU

data Slot = NotificationSlot NotificationLevel
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type NotificationMsgs = Array String

data NotificationLevel
  = Warning
  | Error
  | Success
derive instance eqNotificationLevel :: Eq NotificationLevel
derive instance ordNotificationLevel :: Ord NotificationLevel

type Notification =
   { level :: NotificationLevel
   , msgs :: NotificationMsgs
   }

data Query a
  = Close a
  | HandleInput Notification a

data Message
  = NotififyClose

mkNotification :: NotificationLevel -> NotificationMsgs -> Notification
mkNotification level msgs = { level, msgs }

view
  :: forall m
   . H.Component HH.HTML Query Notification Message m
view =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: Just <<< H.action <<< HandleInput
    }

render :: Notification -> H.ComponentHTML Query
render {level, msgs} =
  let
    colorClazz = case _ of
      Warning -> B.Warning
      Error -> B.Danger
      Success -> B.Success
  in
  HH.section
    [ HU.classNames
      [ B.notification
      , BModifier.isColor $ colorClazz level
      ]
    ]
    [ HH.button
      [ HU.className B.delete
      , HE.onClick (HE.input_ Close) ]
      [ HU.emptyView ]
    , HH.ul_
    $ msgs <#> \msg ->
          HH.li_
          [ HH.text msg ]
    ]

eval :: forall m. Query ~> H.ComponentDSL Notification Query Message m
eval (Close next) = do
  H.raise NotififyClose
  pure next
eval (HandleInput _ next) =
  pure next
