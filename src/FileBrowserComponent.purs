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
import GalleryComponent (isPdf)
import GalleryComponent as Gallery
import Grid as Grid
import Image (PalettizedImage, ScreenWidth)
import Image as Image
import Node.FS.Aff as FS
import Node.FS.Stats as Stats
import Node.Path as Path
import Pdf as Pdf
import Type.Proxy (Proxy(..))
import ZipperArray as ZA

data FileType = Folder | Image | Pdf | Unknown
data File = File FileType String

data Item = FileItem File | PrevItem | NextItem | UpItem | EmptyItem

data BrowserState = BrowserState (ZA.ZipperArray (Grid.Grid Item))


type CellWidth = (D1 :* D9) :* D8

type CellHeight = (D1 :* D0) :* D2 

type CellHeightWOText = (D8 :* D7)

type TextHeight = D1 :* D5


cellWidth :: Proxy CellWidth
cellWidth =Proxy

cellHeight :: Proxy CellHeight
cellHeight = Proxy

header :: Image.Sized Image.ScreenWidth (D2 :* D8) PalettizedImage
header = Image.loadSizedPalettizedImage Proxy Proxy "assets/header.png"

fileImage :: File -> Image.Sized CellWidth CellHeight Image.PalettizedImage
fileImage (File Folder name) = Image.loadSizedPalettizedImage Proxy (Proxy :: Proxy CellHeightWOText) "assets/folder.png" 
                                    `Image.concatV`
                                    Image.renderText Proxy (Proxy :: Proxy TextHeight) (Path.basename name)
fileImage (File Unknown name) = Image.loadSizedPalettizedImage Proxy (Proxy :: Proxy CellHeightWOText) "assets/unknown.png"
                                    `Image.concatV`
                                    Image.renderText Proxy (Proxy :: Proxy TextHeight) (Path.basename name)
fileImage (File Pdf name) = Image.loadSizedPalettizedImage Proxy (Proxy :: Proxy CellHeightWOText) "assets/pdf.png"
                                    `Image.concatV`
                                    Image.renderText Proxy (Proxy :: Proxy TextHeight) (Path.basename name)
fileImage (File Image path) = Image.loadSizedArbitraryImage Proxy Proxy path

itemImage :: Item -> Image.Sized CellWidth CellHeight Image.PalettizedImage
itemImage (FileItem file) = fileImage file
itemImage PrevItem = Image.loadSizedPalettizedImage' "assets/prev.png"
itemImage NextItem = Image.loadSizedPalettizedImage' "assets/next.png"
itemImage UpItem = Image.loadSizedPalettizedImage' "assets/up.png"
itemImage EmptyItem = Image.blank' Image.White


joinImageGrid :: forall t27 t28 t29 t30 t33 t46 t47. Pos t27 => Pos t28 => Pos t29 => Add t28 t29 t30 => Add t29 t28 t30 => Pos t33 => Pos t29 => Add t33 t29 t28 => Add t29 t33 t28 => Pos t29 => Pos t29 => Add t29 t29 t33 => Add t29 t29 t33 => Pos t46 => Pos t47 => Pos t29 => Add t46 t47 t27 => Add t47 t46 t27 => Pos t47 => Pos t47 => Pos t29 => Add t47 t47 t46 => Add t47 t47 t46 => Grid.Grid (Image.Sized t47 t29 Image.PalettizedImage) -> Image.Sized t27 t30 Image.PalettizedImage
--joinGrid :: forall wi hi wo ho. Pos wi => Pos hi => Mul D3 wi wo => Mul D4 hi ho => Grid.Grid (Image.Sized wi hi Image.PalettizedImage) -> Image.Sized wo ho Image.PalettizedImage
joinImageGrid (Grid.Grid v) = let jh (Grid.Horizontal x y z) = x `Image.concatH` y `Image.concatH` z
                                  jv (Grid.Vertical w x y z) = w `Image.concatV` x `Image.concatV` y `Image.concatV` z
                               in jv $ jh <$> v
 

joinImageGridBorder :: forall t38 t39 t40 t41 t44 t45 t49 t54 t59 t72 t73 t77 t78 t82. Pos t38 => Pos t39 => Pos t40 => Add t39 t40 t41 => Add t40 t39 t41 => Pos t44 => Pos t45 => Add t44 t45 t39 => Add t45 t44 t39 => Pos t49 => Pos t40 => Add t49 t40 t44 => Add t40 t49 t44 => Pos t54 => Pos t45 => Add t54 t45 t49 => Add t45 t54 t49 => Pos t59 => Pos t40 => Add t59 t40 t54 => Add t40 t59 t54 => Pos t40 => Pos t45 => Add t40 t45 t59 => Add t45 t40 t59 => Pos t72 => Pos t73 => Pos t40 => Add t72 t73 t38 => Add t73 t72 t38 => Pos t77 => Pos t78 => Pos t40 => Add t77 t78 t72 => Add t78 t77 t72 => Pos t82 => Pos t73 => Pos t40 => Add t82 t73 t77 => Add t73 t82 t77 => Pos t73 => Pos t78 => Pos t40 => Add t73 t78 t82 => Add t78 t73 t82 => Image.Sized t78 t40 PalettizedImage -> Image.Sized t38 t45 PalettizedImage -> Grid.Grid (Image.Sized t73 t40 PalettizedImage) -> Image.Sized t38 t41 PalettizedImage
joinImageGridBorder hBorder vBorder (Grid.Grid v) = 
    let jh (Grid.Horizontal x y z) = x `Image.concatH` hBorder `Image.concatH` y `Image.concatH` hBorder `Image.concatH` z
        jv (Grid.Vertical w x y z) = w `Image.concatV` vBorder `Image.concatV` x `Image.concatV` vBorder `Image.concatV` y `Image.concatV` vBorder `Image.concatV` z
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
    isPdf <- Gallery.isPdf path
    let t = if Stats.isDirectory s 
        then  Folder
        else if isImage
            then Image
            else if isPdf
                then Pdf
                else Unknown
    pure $ File t path


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
    | InDirectory FileBrowserState (EReaderComponent (Maybe FileBrowserState))
    | InGallery FileBrowserState (EReaderComponent (Maybe Gallery.GalleryState))

update :: Tuple Buttons.ButtonId Buttons.PressType -> Maybe FileBrowserState -> Aff (Maybe FileBrowserState)
update _ Nothing = pure Nothing
update p (Just st@(InBrowser dir (BrowserState z))) =
    case indexGrid p (ZA.current z) of
                PrevItem -> pure <<< Just <<< InBrowser dir <<< BrowserState $ fromMaybe (ZA.goLast z) $ ZA.goPrev z
                NextItem -> pure <<< Just <<< InBrowser dir <<< BrowserState $ fromMaybe (ZA.goFirst z) $ ZA.goNext z
                UpItem -> pure $ Nothing
                FileItem (File Image file) -> Just <<< InGallery st <<< doubleTapEscapeableComponent <$> Gallery.galleryComponent dir file
                FileItem (File Folder file) -> Just <<< InDirectory st <$> fileBrowserComponent file
                FileItem (File Pdf file) -> do
                     extractedDir <- Pdf.extractPdf file
                     Just <<< InGallery st <<< doubleTapEscapeableComponent <$> Gallery.galleryComponent extractedDir ""
                _ -> pure <<< Just <<< InBrowser dir $ BrowserState z
update p (Just (InGallery dir component )) = do
    newC <- embedComponentUpdate component p
    case newC.initialState of 
        Just _ -> pure <<< Just <<< InGallery dir $ newC
        Nothing -> pure <<< Just $ dir
update p (Just (InDirectory dir component )) = do
    newC <- embedComponentUpdate component p
    case newC.initialState of 
        Just _ -> pure <<< Just <<< InDirectory dir $ newC
        Nothing -> pure <<< Just $ dir

fileBrowserComponent :: String -> Aff (EReaderComponent (Maybe FileBrowserState))
fileBrowserComponent dir = do
    state <- InBrowser dir <$> makeBrowserState dir 
    let render st = 
          case st of
            (Just (InBrowser _ (BrowserState s))) -> Image.concatV header $ joinImageGridBorder (Image.blank (Proxy :: Proxy D3) (Proxy :: Proxy CellHeight) Image.Black) (Image.blank (Proxy :: Proxy ScreenWidth) (Proxy :: Proxy D4)  Image.Black) $ itemImage <$> (ZA.current s)
            (Just (InGallery _ c)) -> embedComponentRender c
            (Just (InDirectory _ c)) -> embedComponentRender c
            Nothing -> Image.loadSizedPalettizedImage Proxy Proxy "assets/inconsistency.png"
    
    pure $ {
        initialState: Just state,
        updateComponent: update,
        renderComponent: render    
    }
