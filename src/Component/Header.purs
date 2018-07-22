module Component.Header where


import Bulma.Common (Is(Is4, Is1)) as B
import Bulma.Elements.Title as BTitle
import Bulma.Layout.Layout (HeroColor(Light), hero, heroBody, isHeroColor) as B
import Const (repoUrl)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import HalogenUtil as HU

view :: forall p i. HH.HTML p i
view =
  HH.section
            [ HU.classNames
              [ B.hero
              , B.isHeroColor B.Light
              ]
            ]
            [ HH.div
                [ HU.className B.heroBody ]
                [ HH.h1
                    [ HU.classNames
                        [ BTitle.title
                        , BTitle.isSize B.Is1
                        ]
                    ]
                    [ HH.text "Pet Shop" ]
                , HH.h2
                    [ HU.className BTitle.subtitle
                    ]
                    [ HH.h2
                      [ HU.classNames
                          [ BTitle.subtitle
                          , BTitle.isSize B.Is4
                          ]
                      ]
                      [ HH.a
                        [ HP.href "https://ethereum.org/"
                        ]
                        [ HH.text "Ethereum" ]
                      , HH.text " based smart contract example based on "
                      , HH.a
                        [ HP.href "https://github.com/f-o-a-m/cliquebait"
                        ]
                        [ HH.text "Cliquebait" ]
                      , HH.text ", "
                      , HH.a
                        [ HP.href "https://github.com/f-o-a-m/chanterelle"
                        ]
                        [ HH.text "Chanterelle" ]
                      , HH.text ", "
                      , HH.a
                        [ HP.href "https://github.com/f-o-a-m/purescript-web3" ]
                        [ HH.text "purescript-web3" ]
                      , HH.text " and "
                      , HH.a
                        [ HP.href repoUrl ]
                        [ HH.text "other funny things" ]
                      , HH.text "."
                      ]
                    ]
                ]
            ]
