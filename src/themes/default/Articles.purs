module Themes.Default.Articles where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routers as R

data Query a = Unit a

type Input = R.Routes

data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = R.Routes

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: const R.Home
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ComponentHTML Query
    -- render state = HH.div_ [map renderArticles state]
    render state = HH.div_ []

    -- renderArticles = id

    eval :: Query ~> H.ComponentDSL State Query Void m
    eval (Unit a) = pure a
