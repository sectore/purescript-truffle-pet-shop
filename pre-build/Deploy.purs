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
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Network.Ethereum.Web3 (Address, ETH, _from, _gas, defaultTransactionOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, writeTextFile)
import Node.Process (PROCESS)
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff (console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS, exception :: EXCEPTION | e) Unit
main = deployMain deployScript

deployScript :: forall eff. DeployM eff Address
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "4712388"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  adoption <- deployContract txOpts adoptionConfig
  let addr = adoption.deployAddress
  _ <- liftAff $ log $ "address " <> show addr
  writeAddress addr
  pure addr

writeAddress
    :: forall eff m
    . MonadAff (fs :: FS | eff) m
    => Address
    -> m Unit
writeAddress address =
  liftAff $ writeTextFile UTF8 ".env" $ "ADOPTION_ADDRESS = '" <> show address <> "'"

adoptionConfig :: ContractConfig NoArgs
adoptionConfig =
 { filepath : "./build/contracts/Adoption.json"
 , name : "Adoption"
 , constructor : constructorNoArgs
 , unvalidatedArgs : noArgs
 }
