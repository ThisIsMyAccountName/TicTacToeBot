module Tokens where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getToken :: IO T.Text
getToken = TIO.readFile "../keys/auth-token.secret"