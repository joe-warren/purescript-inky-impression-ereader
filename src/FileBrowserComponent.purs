module FileBrowserComponent
  where

import Component
import Data.Tuple
import Data.Typelevel.Num.Reps
import Data.Typelevel.Num.Sets
import Prelude

import Buttons as Buttons
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Typelevel.Num.Ops (class Add, class Mul)
import Effect.Aff (Aff)
import GalleryComponent as Gallery
import Grid as Grid
import Image as Image
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Type.Proxy (Proxy(..))
import ZipperArray as ZA

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
 

indexVertical :: forall a. Buttons.ButtonId -> Grid.Vertical a -> a
indexVertical Buttons.Button1 (Grid.Vertical a _ _ _) = a
indexVertical Buttons.Button2 (Grid.Vertical _ a _ _) = a
indexVertical Buttons.Button3 (Grid.Vertical _ _ a _) = a
indexVertical Buttons.Button4 (Grid.Vertical _ _ _ a) = a

indexHorizontal :: forall a. Buttons.PressType -> Grid.Horizontal a -> a
indexHorizontal Buttons.ShortTap  (Grid.Horizontal a _ _) = a
indexHorizontal Buttons.DoubleTap (Grid.Horizontal _ a _) = a
indexHorizontal Buttons.LongTap   (Grid.Horizontal _ _ a) = a

indexGrid :: forall a. Tuple Buttons.ButtonId Buttons.PressType -> Grid.Grid a -> a
indexGrid (Tuple button press) (Grid.Grid v) = indexHorizontal press $ indexVertical button v

mkFile :: String -> Aff File
mkFile path = do
    s <- FS.stat path
    isImage <- Gallery.isImage path
    pure $ if Stats.isDirectory s 
        then  (File Folder path)
        else if isImage
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
        else let mkOneGrid ar = fromMaybe EmptyItem <$> Grid.fromArray ([PrevItem, NextItem, UpItem] <> (FileItem <$> ar))
              in BrowserState $ fromMaybe emptyBrowserState $ ZA.fromArray (mkOneGrid <$> chunk 9 files)


data FileBrowserState = InBrowser String BrowserState 
    | InDirectory String (EReaderComponent (Maybe FileBrowserState))
    | InGallery String (EReaderComponent (Maybe Gallery.GalleryState))

update :: Tuple Buttons.ButtonId Buttons.PressType -> Maybe FileBrowserState -> Aff (Maybe FileBrowserState)
update _ Nothing = pure Nothing
update p (Just (InBrowser dir (BrowserState z))) =
    case indexGrid p (ZA.current z) of
                PrevItem -> pure <<< Just <<< InBrowser dir <<< BrowserState $ fromMaybe (ZA.goLast z) $ ZA.goPrev z
                NextItem -> pure <<< Just <<< InBrowser dir <<< BrowserState $ fromMaybe (ZA.goFirst z) $ ZA.goNext z
                UpItem -> pure $ Nothing
                FileItem (File Image file) -> Just <<< InGallery dir <<< doubleTapEscapeableComponent <$> Gallery.galleryComponent dir file
                FileItem (File Folder file) -> Just <<< InDirectory dir <$> fileBrowserComponent file
                _ -> pure <<< Just <<< InBrowser dir $ BrowserState z
update p (Just (InGallery dir component )) = do
    newC <- embedComponentUpdate component p
    case newC.initialState of 
        Just _ -> pure <<< Just <<< InGallery dir $ newC
        Nothing -> Just <<< InBrowser dir <$> makeBrowserState dir 
update p (Just (InDirectory dir component )) = do
    newC <- embedComponentUpdate component p
    case newC.initialState of 
        Just _ -> pure <<< Just <<< InDirectory dir $ newC
        Nothing -> Just <<< InBrowser dir <$> makeBrowserState dir 

fileBrowserComponent :: String -> Aff (EReaderComponent (Maybe FileBrowserState))
fileBrowserComponent dir = do
    state <- InBrowser dir <$> makeBrowserState dir 
    let render st = 
          case st of
            (Just (InBrowser _ (BrowserState s))) -> joinImageGrid $ itemImage <$> (ZA.current s)
            (Just (InGallery _ c)) -> embedComponentRender c
            (Just (InDirectory _ c)) -> embedComponentRender c
            Nothing -> Image.loadSizedPalettizedImage Proxy Proxy "assets/inconsistency.png"
    
    pure $ {
        initialState: Just state,
        updateComponent: update,
        renderComponent: render    
    }
