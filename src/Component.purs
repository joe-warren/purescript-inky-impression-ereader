module Component where

import Prelude
import Streams as SR
import Streams ((>->))
import Effect.Aff.Class (class MonadAff)
import Control.Parallel.Class (class Parallel)

type Component ev r m st = {
    initialState :: st,
    updateComponent :: ev -> st -> m st,
    renderComponent :: st -> r
}

componentToStream :: forall m f ev r st a. MonadAff m => Parallel f m => Semigroup a => Component ev r m st -> SR.Stream ev r m a
componentToStream c = SR.statefully (c.updateComponent) c.initialState >-> SR.sMap c.renderComponent


