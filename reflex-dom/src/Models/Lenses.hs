{-# LANGUAGE
    FlexibleInstances
  , FunctionalDependencies
  , MultiParamTypeClasses
  , TemplateHaskell
#-}

module Models.Lenses
  ( module Models.Lenses
  , (^.)
  )
where

import Lens.Micro.Platform (makeLensesWith, camelCaseFields, (^.))

import Models.Auth
import Models.Content
import Models.Errors

-- Models.Errors
makeLensesWith camelCaseFields ''ApiError

-- Models.Auth
makeLensesWith camelCaseFields ''User
makeLensesWith camelCaseFields ''AuthRequest
makeLensesWith camelCaseFields ''AuthResponse
makeLensesWith camelCaseFields ''RegisterRequest

-- Models.Content
makeLensesWith camelCaseFields ''NoteShallow
makeLensesWith camelCaseFields ''GroupShallow
makeLensesWith camelCaseFields ''NewGroupRequest
makeLensesWith camelCaseFields ''GroupResponse
makeLensesWith camelCaseFields ''NewNoteRequest
makeLensesWith camelCaseFields ''NoteResponse
