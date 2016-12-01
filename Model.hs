module Model where

import ClassyPrelude
import Database.Persist.Quasi (upperCaseSettings)
import Database.Persist.TH (mkMigrate, mkPersist, mpsGenerateLenses, mpsGeneric, persistFileWith, share, sqlSettings)

let settings = sqlSettings { mpsGenerateLenses = True, mpsGeneric = False }
 in share [mkPersist settings, mkMigrate "migrateAll"]
    $(persistFileWith upperCaseSettings "models")
