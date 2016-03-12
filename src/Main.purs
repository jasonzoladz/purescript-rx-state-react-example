module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception

import Data.Foldable (foldl, traverse_)

import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)

import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element(), ElementId(..), documentToNonElementParentNode)

import React
import ReactDOM as RD
import React.DOM as D
import React.DOM.Props as P

import Control.Monad.Aff
import Network.HTTP.Affjax
import Network.HTTP.Method (Method(..))
import Network.HTTP.Affjax.Response

import RxState
import Data.Argonaut.Core (Json)

data AppAction
  = Increment
  | Decrement
  | NoOp

data Effect
  = None
  | GetStuff
  | ExternalIncrement


type AppState = { num :: Int }

initState :: AppState
initState = { num: 1 }

actionsChannel :: Channel (Array AppAction)
actionsChannel = newChannel []

effectsChannel :: Channel (Array Effect)
effectsChannel = newChannel []

update :: AppState -> AppAction -> AppState
update state action = do
  case action of
    Increment -> state { num = state.num + 1 }
    Decrement -> state { num = state.num - 1 }
    NoOp      -> state

performEffect :: forall e. Effect -> Eff ( console :: CONSOLE, ajax :: AJAX | e) Unit
performEffect fx =
  case fx of
    GetStuff ->          log "Getting Stuff :) !"
    ExternalIncrement -> runAff
                            (\_ -> send [ Increment ] actionsChannel)
                            (\_ -> send [ Increment ] actionsChannel)
                            ((affjax $ defaultRequest { url = "/api", method = GET }) :: forall e. Aff (ajax :: AJAX, console :: CONSOLE | e) (AffjaxResponse Json))

    _        ->          log "Doing other things."

hello :: ReactClass AppState
hello = createClass $ spec unit $ \ctx -> do
  state <- getProps ctx
  return $
    D.div [] [ D.h1 []
                  [ D.text "Hello, the state is: "
                  , D.text (show state.num)
                  ],
               D.div []
                  [ D.button [ P.onClick (\ evt -> do
                                              send [Increment] actionsChannel) ]
                             [ D.text "Increment" ]
                  , D.button [ P.onClick \_ -> send [Decrement] actionsChannel ]
                             [ D.text "Decrement" ]
                  , D.button [ P.onClick \_ -> send [ExternalIncrement] effectsChannel ]
                             [ D.text "Ajax Increment" ]
                  ]

             ]

main :: forall eff. Eff (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff ) Unit
main = startApp update performEffect myRender actionsChannel effectsChannel initState

  where
    view :: AppState -> ReactElement
    view appState = D.div' [ createFactory hello appState ]

    myRender state = do
      container <- elm'
      RD.render (view state) container

    elm' :: forall eff. Eff (dom :: DOM | eff) Element
    elm' = do
      win <- window
      doc <- document win
      elm <- getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))
      return $ fromJust (toMaybe elm)
