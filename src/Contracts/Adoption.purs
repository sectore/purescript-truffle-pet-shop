--------------------------------------------------------------------------------
-- | Adoption
--------------------------------------------------------------------------------

module Contracts.Adoption where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, call, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (D2, D5, D6, Tuple0(..), Tuple1(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (N16, type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | GetAdoptersFn
--------------------------------------------------------------------------------


type GetAdoptersFn = Tagged (SProxy "getAdopters()") (Tuple0 )

getAdopters :: forall e. TransactionOptions NoPay -> ChainCursor -> Web3 e (Either CallError (Vector N16 Address))
getAdopters x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: GetAdoptersFn)

--------------------------------------------------------------------------------
-- | AdoptersFn
--------------------------------------------------------------------------------


type AdoptersFn = Tagged (SProxy "adopters(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

adopters :: forall e. TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& D6)) -> Web3 e (Either CallError Address)
adopters x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AdoptersFn)

--------------------------------------------------------------------------------
-- | AdoptFn
--------------------------------------------------------------------------------


type AdoptFn = Tagged (SProxy "adopt(uint256)") (Tuple1 (UIntN (D2 :& D5 :& D6)))

adopt :: forall e. TransactionOptions NoPay -> { petId :: (UIntN (D2 :& D5 :& D6)) } -> Web3 e HexString
adopt x0 r = uncurryFields  r $ adopt' x0
   where
    adopt' :: TransactionOptions NoPay -> Tagged (SProxy "petId") (UIntN (D2 :& D5 :& D6)) -> Web3 e HexString
    adopt' y0 y1 = sendTx y0 ((tagged $ Tuple1 (untagged y1 )) :: AdoptFn)
