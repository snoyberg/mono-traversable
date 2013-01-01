{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Yesod
    ( module X
    , module ClassyPrelude.Yesod
    ) where

import ClassyPrelude.Conduit as X hiding (lift)
import Yesod as X hiding (Request, Header, insert, delete)
import qualified Yesod
import Yesod.Static as X
import Yesod.Feed as X
import Network.HTTP.Conduit as X
import Network.HTTP.Types as X
import qualified ClassyPrelude.Classes
import Database.Persist.GenericSql.Raw as X (SqlBackend, SqlPersist)
import Database.Persist.GenericSql.Migration as X (runMigration)

instance
  ( backend ~ PersistMonadBackend m
  , backend ~ PersistEntityBackend entity
  , PersistStore m
  , PersistEntity entity
  , entity ~ entity'
  ) => ClassyPrelude.Classes.CanInsert (entity -> m (KeyBackend backend entity')) where
    insert = Yesod.insert

-- | Non-typeclass insert for use with Persistent.
--
-- If you don\'t use the @Key@ returned from @insert@, the type inferencing
-- cannot tell which @insert@ function to use. Using @insertDB@ disambiguates.
-- Another options is using 'voidKey'.
--
-- Since 0.1.0.0
insertDB :: ( PersistStore m
            , PersistEntity entity
            , PersistMonadBackend m ~ PersistEntityBackend entity
            )
         => entity
         -> m (Key entity)
insertDB = Yesod.insert

-- | Ignore the @Key@ returned by insert.
--
-- See 'insertDB' for more information. The following two lines are equivalent:
--
-- > _ <- insertDB foo
-- > voidKey $ insert foo
--
-- Since 0.1.0.0
voidKey :: Functor m => m (KeyBackend backend entity) -> m ()
voidKey = void

instance
  ( backend ~ PersistMonadBackend m
  , backend ~ PersistEntityBackend entity
  , PersistStore m
  , PersistEntity entity
  , a ~ ()
  ) => ClassyPrelude.Classes.CanDelete (KeyBackend backend entity -> m a) where
    delete = Yesod.delete
