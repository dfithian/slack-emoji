module Foundation (module Foundation) where

import Settings (AppSettings)

data App = App
  { appSettings :: AppSettings
  }
