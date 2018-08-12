module Environment where

import Config (adaptionAddress)
import Network.Ethereum.Core.Signatures (Address)

type Env =
  { contractAddress :: Address
  }

mkEnv :: Env
mkEnv =
  { contractAddress: adaptionAddress
  }
