module Main where

import Data.Void
import Data.Tuple
import Data.Either

import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A

import Debug.Trace

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
data State = State Number

-- | Inputs to the state machine
data Input = Up | Down

instance showInput :: Show Input where
  show Up = "Up"
  show Down = "Down"

ui :: forall m eff. (Applicative m) => Component m Input Input
ui = render <$> stateful (State 0) update
  where
  render :: State -> H.HTML (m Input)
  render (State n) =
    H.div_
          [ H.p_ [ H.text (show n) ]
          , H.div_
            [ H.button [ A.onClick (A.input_ Down ) ] [ H.text "-"]
            , H.button [ A.onClick (A.input_ Up) ] [ H.text "+" ]
            ]
          ]

  update :: State -> Input -> State
  update (State n) Up = State (n + 1)
  update (State n) Down = State (n - 1)

combined :: forall m. (Applicative m) => Component m (Either Input Input) (Either Input Input)
combined = combine (\a b -> H.div_ [a, b]) ui ui

main = do
  Tuple node _ <- runUI combined
  appendToBody node
  Tuple node' _ <- runUIWith ui (\req elt driver -> do
    str <- innerHTML elt
    trace str)
  appendToBody node'
