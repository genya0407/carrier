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
    arch :: Maybe T.Text,
    environments :: M.Map T.Text T.Text,
    images :: M.Map T.Text T.Text,
    registry :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON Config

instance FromJSON Config

{-
version: '3'
services:
  web:
    image: docker.genya0407.net/reing_web:${TAG}
    command: cargo run --release
    ports:
      - "127.0.0.1:${PORT:?err}:3000"
    restart: always
    links:
      - postgres
    env_file: .env
    depends_on:
      - postgres
  postgres:
    image: postgres:12-alpine
    restart: always
    env_file: .env
    volumes:
      - reing_pg_data:/var/lib/postgresql/data

volumes:
  reing_pg_data:
    external: true

-}