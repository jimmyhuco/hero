module Themes.Default.Header where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a = Unit a

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = Unit

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state = HH.nav
                   [ HP.classes (map HH.ClassName [ "navbar"
                                                  , "is-fixed-top"
                                                  , "is-transparent"]) ]
                   [ HH.div
                    [ HP.class_ (HH.ClassName "navbar-brand") ]
                    [ HH.a
                      [ HP.class_ (HH.ClassName "navbar-item") ]
                      [ HH.img [ HP.src "images/logo.png"
                               , HP.alt "Haskell Cafe"
                               , HP.width 28
                               ] ]
                    , HH.button
                      [ HP.classes (map HH.ClassName [ "button"
                                                     , "navbar-burger" ])
                      , HP.attr (H.AttrName "data-target") "navMenu" ]
                      [ HH.span_ []
                      , HH.span_ []
                      ] ]

                   , HH.div
                     [ HP.class_ (HH.ClassName "navbar-menu")
                     , HP.id_ "navMenu" ]
                     [ HH.div
                       [ HP.class_ (HH.ClassName "navbar-start") ]
                       [ HH.a
                         [ HP.class_ (HH.ClassName "navbar-item")
                         , HP.href "#/" ]
                         [ HH.text "Home" ]
                       , HH.a
                         [ HP.class_ (HH.ClassName "navbar-item")
                         , HP.href "#/about" ]
                         [ HH.text "About" ]
                       ] ]
                   ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Unit a) = pure a
