module Chapter18 where

import           Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f
