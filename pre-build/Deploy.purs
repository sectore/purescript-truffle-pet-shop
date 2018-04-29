module Deploy where

import Prelude

import Chanterelle (deployMain)
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployM, constructorNoArgs)
import Chanterelle.Internal.Types.Deploy (NoArgs, noArgs)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Network.Ethereum.Core.BigNumber (embed)
import Network.Ethereum.Web3 (Address, ETH, _from, _gas, defaultTransactionOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, writeTextFile)
import Node.Process (PROCESS)

main :: forall e. Eff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS, exception :: EXCEPTION | e) Unit
main = deployMain deployScript

type DeployResults =
  ( adoptionAddr :: Address
  )

deployScript :: forall eff. DeployM eff (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ embed 4712388
  adoption <- deployContract txOpts adoptionConfig
  let adAddr = adoption.deployAddress
  _ <- liftAff $ log $ "adoption address " <> show adAddr
  writeAddresses adAddr
  pure { adoptionAddr: adAddr }

writeAddresses
    :: forall eff m
    . MonadAff (fs :: FS | eff) m
    => Address
    -> m Unit
writeAddresses address =
  liftAff $ writeTextFile UTF8 ".env" $ "ADOPTION_ADDRESS = '" <> show address <> "'"

adoptionConfig :: ContractConfig NoArgs
adoptionConfig =
 { filepath : "./build/Adoption.json"
 , name : "Adoption"
 , constructor : constructorNoArgs
 , unvalidatedArgs : noArgs
 }
