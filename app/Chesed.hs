{-# LANGUAGE OverloadedStrings #-}
module Chesed where

import Control.Monad (unless, void)
import qualified Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified SDL.Image as IMG
import qualified SDL.Font as TTF
import qualified Sound.ALUT as ALUT

import Text.Read (readMaybe)

import Parser.ScriptParser
import Parser.ScriptAst

import Text.Megaparsec (parse, errorBundlePretty)
import System.Exit (exitFailure, exitSuccess)
import System.IO (readFile)
import Paths_Chesed (getDataFileName)


data GameState = GameState
  { commands          :: [VNCommand]
  , currentIndex      :: Int
  , currentDialog     :: String
  , currentBackground :: Maybe SDL.Texture
  , currentCharacter  :: Maybe SDL.Texture
  , characterRect     :: Maybe (SDL.Rectangle CInt)
  , winWidth          :: CInt
  , winHeight         :: CInt
  , currentMusic      :: Maybe ALUT.Source
  }

approxTextWidth :: String -> Float
approxTextWidth s = fromIntegral (length s) * avgCharWidth
  where
    avgCharWidth = 11

wrapWords :: Float -> [String] -> [String]
wrapWords _ [] = []
wrapWords maxWidth ws = go ws ""
  where
    go [] currentLine = [currentLine]
    go (w:ws) currentLine =
      let testLine = if null currentLine then w else currentLine ++ " " ++ w
      in if approxTextWidth testLine <= maxWidth
           then go ws testLine
           else currentLine : go (w:ws) ""

wrapText :: Float -> String -> String
wrapText maxWidth s = unlines $ wrapWords maxWidth (words s)

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture rdr path = do
  fullPath <- getDataFileName path
  IMG.loadTexture rdr fullPath

processNext :: SDL.Renderer -> GameState -> IO GameState
processNext rdr gs
  | currentIndex gs >= length (commands gs) = return gs
  | otherwise =
      case commands gs !! currentIndex gs of
        VNSay speaker dialogue -> do
          let newDialog = if speaker == "Narrator "
                          then dialogue
                          else speaker ++ "\n\n" ++ dialogue
          return gs { currentDialog = newDialog
                    , currentIndex = currentIndex gs + 1 }
        VNSetBackground path -> do
          tex <- loadTexture rdr ("game/images/" ++ path)
          processNext rdr gs { currentBackground = Just tex
                             , currentIndex = currentIndex gs + 1 }

        VNShowCharacter _ posx posy sx sy path -> do
          tex <- loadTexture rdr ("game/images/" ++ path)
          case (readMaybe posx :: Maybe CInt,
                readMaybe posy :: Maybe CInt,
                readMaybe sx   :: Maybe CInt,
                readMaybe sy   :: Maybe CInt) of
            (Just x, Just y, Just width, Just height) -> do
                let destRect = SDL.Rectangle (SDL.P (V2 x y)) (V2 width height)
                processNext rdr gs { currentCharacter = Just tex
                                        , characterRect = Just destRect
                                        , currentIndex = currentIndex gs + 1 }
            _ -> error $ "Failed to parse numeric values: " ++ show (posx, posy, sx, sy)

        VNHide ->
          processNext rdr gs { currentCharacter = Nothing
                             , currentIndex = currentIndex gs + 1 }

        VNMusic path -> do
          newState <- playMusicALUT path gs
          processNext rdr (newState { currentIndex = currentIndex newState + 1 })

        VNStopMusic -> do
          stopMusicALUT gs
          processNext rdr (gs { currentIndex = currentIndex gs + 1 })

        _ ->
          processNext rdr gs { currentIndex = currentIndex gs + 1 }

initialGameState :: CInt -> CInt -> SDL.Renderer -> [VNCommand] -> IO GameState
initialGameState w h rdr cmds =
  processNext rdr GameState
    { commands          = cmds
    , currentIndex      = 0
    , currentDialog     = ""
    , currentBackground = Nothing
    , currentCharacter  = Nothing
    , winWidth          = w
    , winHeight         = h
    , currentMusic      = Nothing
    }


playMusicALUT :: FilePath -> GameState -> IO GameState
playMusicALUT relPath gs = do
  fullPath <- getDataFileName ("game/music/" ++ relPath ++ ".wav")
  putStrLn $ "Loading music from: " ++ fullPath
  buffer <- ALUT.createBuffer (ALUT.File fullPath)
  source <- ALUT.genObjectName
  ALUT.buffer source ALUT.$= Just buffer
  ALUT.play [source]
  return gs { currentMusic = Just source }


stopMusicALUT :: GameState -> IO ()
stopMusicALUT gs =
  case currentMusic gs of
    Just source -> do
      ALUT.stop [source]
    Nothing ->
      return ()


renderText :: SDL.Renderer -> TTF.Font -> String -> V2 CInt -> IO ()
renderText rdr font str (V2 x y) = do
  let ls = lines str 
  mapM_ (renderLine rdr font x) (zip ls [0..])
  where
    renderLine :: SDL.Renderer -> TTF.Font -> CInt -> (String, Int) -> IO ()
    renderLine rdr font x (line, i) = do
      surface <- TTF.solid font (SDL.V4 255 255 255 255) (T.pack line)
      texture <- SDL.createTextureFromSurface rdr surface
      dim <- SDL.surfaceDimensions surface
      let SDL.V2 w h = dim
          dstRect = SDL.Rectangle (SDL.P (V2 x (y + fromIntegral i * h))) (V2 w h)
      SDL.freeSurface surface
      SDL.copy rdr texture Nothing (Just dstRect)
      SDL.destroyTexture texture


renderGame :: SDL.Renderer -> TTF.Font -> GameState -> IO ()
renderGame rdr font gs = do
  SDL.rendererDrawColor rdr SDL.$= V4 0 0 0 255
  SDL.clear rdr

  case currentBackground gs of
    Just tex ->
      SDL.copy rdr tex Nothing (Just (SDL.Rectangle (SDL.P (V2 0 0))
                                                   (V2 (winWidth gs) (winHeight gs))))
    Nothing -> return ()

  case (currentCharacter gs, characterRect gs) of
    (Just tex, Just rect) -> SDL.copy rdr tex Nothing (Just rect)
    _ -> return ()

  let overlayRect = SDL.Rectangle (SDL.P (V2 0 (winHeight gs - 150)))
                                  (V2 (winWidth gs) 150)
  SDL.rendererDrawColor rdr SDL.$= V4 0 0 0 200
  SDL.fillRect rdr (Just overlayRect)

  let wrappedDialog = if null (currentDialog gs)
                        then "No dialogue."
                        else wrapText 700 (currentDialog gs)
      textPos = V2 20 (winHeight gs - 130)
  renderText rdr font wrappedDialog textPos

  SDL.present rdr

isAdvanceEvent :: SDL.EventPayload -> Bool
isAdvanceEvent (SDL.KeyboardEvent e) =
  SDL.keyboardEventKeyMotion e == SDL.Pressed &&
  SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeSpace
isAdvanceEvent (SDL.MouseButtonEvent e) =
  SDL.mouseButtonEventMotion e == SDL.Pressed &&
  SDL.mouseButtonEventButton e == SDL.ButtonLeft
isAdvanceEvent _ = False

handleResize :: GameState -> SDL.Event -> GameState
handleResize gs event =
  case SDL.eventPayload event of
    SDL.WindowResizedEvent (SDL.WindowResizedEventData _ (V2 w h)) ->
      gs { winWidth = fromIntegral w, winHeight = fromIntegral h }
    _ -> gs

appLoop :: SDL.Renderer -> TTF.Font -> GameState -> IO ()
appLoop rdr font gs = do
  events <- SDL.pollEvents
  let quit = any (isQuit . SDL.eventPayload) events
      advance = any isAdvanceEvent (map SDL.eventPayload events)
  gs' <- if advance then processNext rdr gs else return gs
  let gs'' = foldl handleResize gs' events
  renderGame rdr font gs''
  SDL.delay 16
  unless quit (appLoop rdr font gs'')

isQuit :: SDL.EventPayload -> Bool
isQuit SDL.QuitEvent = True
isQuit _           = False

run :: String -> CInt -> CInt -> IO ()
run windowName width height = ALUT.withProgNameAndArgs ALUT.runALUT $ \_ _ -> do
  E.bracket_ SDL.initializeAll SDL.quit $ do
    IMG.initialize [IMG.InitPNG]
    TTF.initialize

    window <- SDL.createWindow (T.pack windowName) SDL.defaultWindow { SDL.windowInitialSize = V2 width height }
    rdr    <- SDL.createRenderer window (-1) SDL.defaultRenderer

    font <- TTF.load "game/gui/dialog-font.ttf" 24

    scriptPath <- getDataFileName "game/script.chesed"

    scriptContent <- readFile scriptPath
    putStrLn "Raw script content:"
    putStrLn scriptContent

    cmds <- case parse parseScript scriptPath scriptContent of
              Left err -> putStrLn (errorBundlePretty err) >> exitFailure
              Right cs -> do
                putStrLn (show cs)
                return cs

    initState <- initialGameState width height rdr cmds
    appLoop rdr font initState

    TTF.quit
    IMG.quit