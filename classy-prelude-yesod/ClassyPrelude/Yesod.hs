{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module ClassyPrelude.Yesod
    ( module X
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

instance
  ( backend ~ PersistMonadBackend m
  , backend ~ PersistEntityBackend entity
  , PersistStore m
  , PersistEntity entity
  , a ~ ()
  ) => ClassyPrelude.Classes.CanDelete (KeyBackend backend entity -> m a) where
    delete = Yesod.delete
