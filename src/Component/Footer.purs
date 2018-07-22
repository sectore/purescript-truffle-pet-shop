module Component.Footer where

import Bulma.Elements.Elements (content) as B
import Bulma.Layout.Layout (container, footer) as B
import Bulma.Modifiers.Typography as BT
import Const (repoUrl)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenUtil as HU

view :: forall p i . HH.HTML p i
view =
  HH.footer
    [ HU.className B.footer ]
    [ HH.div
      [ HU.className B.container ]
      [ HH.p
          [ HU.classNames [ B.content, BT.hasAlignment BT.Centered ]
          ]
          [ HH.text "All sources are available at "
          , HH.a
              [ HP.href repoUrl
              ]
              [ HH.text "GitHub" ]
          , HH.text "."
          ]
      ]
    ]
