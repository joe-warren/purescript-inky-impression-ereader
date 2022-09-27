module FileBrowserComponent
  where

import Prelude
import Component 
import ZipperArray as ZA

import Effect.Aff (Aff)
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Grid as Grid
import Data.Array as Array
import Data.String as String
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse)


data FileType = Folder | Image | Unknown
data File = File FileType String

data Item = FileItem File | PrevItem | NextItem | UpItem | EmptyItem

data BrowserState = BrowserState (ZA.ZipperArray (Grid.Grid Item))

mkFile :: String -> Aff File
mkFile path = do
    s <- FS.stat path
    pure $ if Stats.isDirectory s 
        then (File Folder path)
        else if String.contains (String.Pattern "\\.png$") path
            then (File Image path)
            else (File Unknown path)


chunk :: forall a. Int -> Array a -> Array (Array a)
chunk n v = let rem = Array.drop n v
                rest = if Array.null rem 
                            then []
                            else chunk n rem
             in Array.cons (Array.take n v) rest

makeBrowserState :: String -> Aff (BrowserState)
makeBrowserState dir = do
    rawNames <- FS.readdir dir
    let fullPaths = ((dir <> "/") <> _) <$> rawNames
    files <- traverse mkFile fullPaths
    let emptyBrowserState = ZA.singleton (pure UpItem)
    pure if Array.length files < 12
        then BrowserState $ ZA.singleton $ fromMaybe EmptyItem <$> Grid.fromArray (Array.cons UpItem (FileItem <$> files))
        else let mkOneGrid ar = fromMaybe EmptyItem <$> Grid.fromArray ([UpItem, PrevItem, NextItem] <> (FileItem <$> ar))
              in BrowserState $ fromMaybe emptyBrowserState $ ZA.fromArray (mkOneGrid <$> chunk 9 files)



data FileBrowserState = InBrowser String BrowserState 
    | InDirectory String FileBrowserState
    | InGallery String (ZA.ZipperArray String)

--fileBrowserComponent :: String -> Aff (EReaderComponent (Maybe FileBrowserState))
