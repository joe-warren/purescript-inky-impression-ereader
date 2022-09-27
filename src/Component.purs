module Component where

import Data.Tuple
import Prelude

import Buttons as Buttons
import Control.Parallel.Class (class Parallel)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Image as Image
import Streams ((>->))
import Streams as SR
import Type.Proxy (Proxy(..))

type Component ev r m st = {
    initialState :: st,
    updateComponent :: ev -> st -> m st,
    renderComponent :: st -> r
}

type EReaderComponent st = Component (Tuple Buttons.ButtonId Buttons.PressType) Image.FullscreenImage Aff st

componentToStream :: forall m f ev r st a. MonadAff m => Parallel f m => Semigroup a => Component ev r m st -> SR.Stream ev r m a
componentToStream c = SR.statefully (c.updateComponent) c.initialState >-> SR.sMap c.renderComponent

embedComponentUpdate :: forall m ev r st. Functor m => Component ev r m st -> ev -> m (Component ev r m st) 
embedComponentUpdate c e = c {initialState = _ } <$> c.updateComponent e c.initialState

embedComponentRender :: forall m ev r st. Component ev r m st -> r
embedComponentRender c = c.renderComponent c.initialState

doubleTapEscapeableComponent :: forall a. EReaderComponent a -> EReaderComponent (Maybe a)
doubleTapEscapeableComponent c = 
    let update (Tuple _ Buttons.DoubleTap) _ = pure Nothing
        update e Nothing = pure Nothing
        update e (Just st) = Just <$> c.updateComponent e st
    in {
        initialState: Just c.initialState,
        updateComponent: update, 
        renderComponent: maybe (Image.loadSizedPalettizedImage Proxy Proxy "assets/inconsistency.png") c.renderComponent
    }


