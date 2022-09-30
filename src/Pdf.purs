module Pdf where

import Prelude

import Data.UUID as UUID
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess as Proc
import Node.FS.Aff as FS

extractPdf :: String -> Aff String
extractPdf path = do
  u <- liftEffect (UUID.genUUID)
  let dir = "/tmp/" <> UUID.toString u
  FS.mkdir $ dir
  log $ "Extracting " <> path <> " to " <> dir
  void $ liftEffect $ Proc.execFileSync "convert" [
        "-density", "100",
        path,
        "-background", "#FFFFFF",
        "-bordercolor", "#FFFFFF",
        "-border", "1x1", 
        "-alpha", "background",
        "-alpha", "off", 
        "-trim",
        "-border", "1x1", 
        "+adjoin",
         dir <> "/output-%03d.png"
     ] Proc.defaultExecSyncOptions
  log $ "Done extracting " <> path <> " to " <> dir
  pure dir
