module Types where


import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Network.Ethereum.Web3 (ETH)
import Network.HTTP.Affjax (AJAX)


type Fx =
  ( avar :: AVAR
  , ref :: REF
  , dom :: DOM
  , ajax :: AJAX
  , console :: CONSOLE
  , eth :: ETH
  , exception :: EXCEPTION
  )
