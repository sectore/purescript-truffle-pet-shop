module HalogenUtil where

import Prelude

import Bulma.Common as B
import Data.Maybe (Maybe, maybe)
import Data.Monoid (mempty)
import Halogen (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

className
  :: forall r i. B.ClassName
  -> HP.IProp ( "class" :: String | r) i
className =
    HP.class_ <<< ClassName <<< B.runClassName

classNames
  :: forall r i. Array B.ClassName
  -> HP.IProp ( "class" :: String | r) i
classNames =
  HP.class_ <<< ClassName <<< B.runClassNames

maybeView :: forall v p i. Maybe v -> (v -> HH.HTML p i) -> HH.HTML p i
maybeView value view = maybe emptyView view value

emptyView :: forall p i. HH.HTML p i
emptyView = HH.text mempty
