module Config (Config (..)) where

import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics (Generic)

data Config = Config
  { projectName :: T.Text,
    port :: T.Text,
    tag :: T.Text,
    context :: T.Text,
    environments :: M.Map T.Text T.Text
  }
  deriving (Generic, Show)

instance ToJSON Config

instance FromJSON Config
