module Themes.Default.Article where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a = Unit a

data Input = R.Routes

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = Unit

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: const unit
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    render state = HH.div_ []

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Unit a) = pure a
