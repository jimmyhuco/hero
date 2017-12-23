module Themes.Default.Layout where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (Eff)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML as HH
import Routers as R
import Routing (matches)
import Themes.Default.Body as Body
import Themes.Default.Footer as Footer
import Themes.Default.Header as Header

data Query a = GOTO R.Routes a

type ChildQuery = Coproduct3 Header.Query Body.Query Footer.Query
type ChildSlot = Either3 Header.Slot Body.Slot Footer.Slot

type State = R.Routes

initialState :: State
initialState = R.Home

component :: forall m. H.Component HH.HTML Query Unit Void m
component = H.parentComponent
  { initialState: const initialState
  , render: render
  , eval: eval
  , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render state = HH.div_ [ HH.slot' cp1 Header.Slot Header.component unit absurd --header
                           , HH.slot' cp2 Body.Slot Body.component state absurd --body
                           , HH.slot' cp3 Footer.Slot Footer.component unit absurd --footer
                           ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
    eval (GOTO route next) = do
      H.put route
      pure next

matchRoutes :: forall eff. H.HalogenIO Query Void (Aff (HA.HalogenEffects eff))
          -> Eff (HA.HalogenEffects eff) Unit
matchRoutes app = matches R.routing (redirects app)
  where
    redirects driver _ = launchAff_ <<< driver.query <<< H.action <<< GOTO
