module Themes.Default.Body where


import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Halogen.Component.ChildPath (cp1, cp2, cp3)

import Routers as R
import Themes.Default.Articles as Articles
import Themes.Default.Article as Article
import Themes.Default.Page as Page

data Query a = Unit a

type ChildQuery = Coproduct3 Articles.Query Article.Query Page.Query
type ChildSlot = Either3 Articles.Slot Article.Slot Page.Slot


data Slot = Slot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

type State = R.Routes

type Input = R.Routes

initialState :: State
initialState = R.Articles

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state = HH.section
                   [ HP.class_ (HH.ClassName "columns") ]
                   [ HH.div
                     [ HP.classes (map HH.ClassName [ "column"
                                                    , "is-half"
                                                    , "is-offset-one-quarter"
                                                    , "articles" ]) ]
                     [ HH.slot' cp1 Articles.Slot Articles.component state absurd ]
                   ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (Unit a) = pure a
