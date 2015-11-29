
module Api.Types where

import qualified Data.ByteString.Char8 as B
import Snap.Snaplet

type Route baseStateType serviceType serviceArgType = (B.ByteString, (Handler baseStateType serviceType serviceArgType))
