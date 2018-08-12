module Config where


import Prelude

import Control.Error.Util (hush)
import Control.Monad.Except (runExcept)
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe(..))
import Network.Ethereum.Web3 (Address, mkAddress, mkHexString)
import Partial.Unsafe (unsafeCrashWith)


foreign import adaptionAddressImpl :: Foreign

adaptionAddress :: Address
adaptionAddress =
  let mAddress =
        (hush $ runExcept $ readString adaptionAddressImpl)
          >>= mkHexString
            >>= mkAddress
  in
    case mAddress of
      Just address -> address
      Nothing -> unsafeCrashWith $
        "Missing environment variable `ADOPTION_ADDRESS` - check README for instruction."

repoUrl :: String
repoUrl = "https://github.com/sectore/purescript-truffle-pet-shop"
