module Themes.Default.Footer where

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
    render state = HH.footer
                   [ HP.class_ (HH.ClassName "footer") ]
                   [ HH.div
                     [ HP.class_ (HH.ClassName "container") ]
                     [ HH.div
                       [ HP.classes (map HH.ClassName [ "content"
                                                      , "has-text-centered" ]) ]
                       [ HH.p_
                         [ HH.text "The website content is licensed "
                         , HH.a
                           [ HP.href "http://creativecommons.org/licenses/by-nc-sa/4.0/" ]
                           [ HH.text "CC BY NC SA 4.0"] ]
                       ] ]
                   ]

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Unit a) = pure a
