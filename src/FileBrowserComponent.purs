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
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Traversable (traverse)
import Type.Proxy (Proxy (..))

import  Data.Typelevel.Num.Sets 
import  Data.Typelevel.Num.Reps
import  Data.Typelevel.Num.Ops (class Add, class Mul)
import Image as Image

data FileType = Folder | Image | Unknown
data File = File FileType String

data Item = FileItem File | PrevItem | NextItem | UpItem | EmptyItem

data BrowserState = BrowserState (ZA.ZipperArray (Grid.Grid Item))


type CellWidth = (D2 :* D0) :* D0

type CellHeight = (D1 :* D1) :* D2

cellWidth :: Proxy CellWidth
cellWidth =Proxy

cellHeight :: Proxy CellHeight
cellHeight = Proxy

fileImage :: File -> Image.Sized CellWidth CellHeight Image.PalettizedImage
fileImage (File Folder _) = Image.loadSizedPalettizedImage Proxy Proxy "assets/folder.png"
fileImage (File Unknown _) = Image.loadSizedPalettizedImage Proxy Proxy "assets/unknown.png"
fileImage (File Image path) = Image.loadSizedArbitraryImage Proxy Proxy path

itemImage :: Item -> Image.Sized CellWidth CellHeight Image.PalettizedImage
itemImage (FileItem file) = fileImage file
itemImage PrevItem = Image.loadSizedPalettizedImage Proxy Proxy "assets/prev.png"
itemImage NextItem = Image.loadSizedPalettizedImage Proxy Proxy "assets/next.png"
itemImage UpItem = Image.loadSizedPalettizedImage Proxy Proxy "assets/up.png"
itemImage EmptyItem = Image.loadSizedPalettizedImage Proxy Proxy "assets/empty.png"


joinImageGrid :: forall t27 t28 t29 t30 t33 t46 t47. Pos t27 => Pos t28 => Pos t29 => Add t28 t29 t30 => Add t29 t28 t30 => Pos t33 => Pos t29 => Add t33 t29 t28 => Add t29 t33 t28 => Pos t29 => Pos t29 => Add t29 t29 t33 => Add t29 t29 t33 => Pos t46 => Pos t47 => Pos t29 => Add t46 t47 t27 => Add t47 t46 t27 => Pos t47 => Pos t47 => Pos t29 => Add t47 t47 t46 => Add t47 t47 t46 => Grid.Grid (Image.Sized t47 t29 Image.PalettizedImage) -> Image.Sized t27 t30 Image.PalettizedImage
--joinGrid :: forall wi hi wo ho. Pos wi => Pos hi => Mul D3 wi wo => Mul D4 hi ho => Grid.Grid (Image.Sized wi hi Image.PalettizedImage) -> Image.Sized wo ho Image.PalettizedImage
joinImageGrid (Grid.Grid v) = let jh (Grid.Horizontal x y z) = x `Image.concatH` y `Image.concatH` z
                                  jv (Grid.Vertical w x y z) = w `Image.concatV` x `Image.concatV` y `Image.concatV` z
                               in jv $ jh <$> v
 


mkFile :: String -> Aff File
mkFile path = do
    s <- FS.stat path
    pure $ if Stats.isDirectory s 
        then (File Folder path)
        else if String.contains (String.Pattern ".png") path
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

fileBrowserComponent :: String -> Aff (EReaderComponent (Maybe BrowserState))
fileBrowserComponent dir = do
    state <- makeBrowserState dir 
    let update _ st = pure st
    let render st = 
          case st of
            (Just (BrowserState s)) -> joinImageGrid $ itemImage <$> (ZA.current s)
            Nothing -> Image.loadSizedPalettizedImage Proxy Proxy "assets/inconsistency.png"
    
    pure $ {
        initialState: Just state,
        updateComponent: update,
        renderComponent: render    
    }
