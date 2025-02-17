{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Juicy

import Parser.ScriptParser
import Parser.ScriptAst

import Text.Megaparsec (parse, errorBundlePretty)
import System.Exit (exitFailure)
import System.IO (readFile)

import Paths_Chesed (getDataFileName)

data GameState = GameState { 
    commands          :: [VNCommand],
    currentIndex      :: Int,
    currentDialog     :: String,
    currentBackground :: Picture,
    currentCharacter  :: Picture,
    winWidth          :: Float,
    winHeight         :: Float
}

approxTextWidth :: String -> Float
approxTextWidth s = fromIntegral (length s) * avgCharWidth
  where
    avgCharWidth = 13

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

initialGameState :: [VNCommand] -> IO GameState
initialGameState cmds = processNext (GameState cmds 0 "" Blank Blank 800 600)

processNext :: GameState -> IO GameState
processNext gs
    | currentIndex gs >= length (commands gs) = return gs
    | otherwise =
        case commands gs !! currentIndex gs of
            VNSay speaker dialogue -> do
                let newDialog = if speaker == "Narrator "
                                then dialogue
                                else speaker ++ ": " ++ dialogue
                return gs { currentDialog = newDialog, currentIndex = currentIndex gs + 1 }

            VNSetBackground path -> do
                bg <- loadImageIO path
                processNext gs { currentBackground = bg, currentIndex = currentIndex gs + 1 }

            VNShowCharacter _ posx posy sx sy path -> do
                charPic <- loadImageIO path
                let transformed = translate (read posx :: Float) (read posy :: Float) $
                                  scale (read sx :: Float) (read sy :: Float) charPic
                processNext gs { currentCharacter = transformed, currentIndex = currentIndex gs + 1 }

            VNHide ->
                processNext gs { currentCharacter = Blank, currentIndex = currentIndex gs + 1 }

            _ ->
                processNext gs { currentIndex = currentIndex gs + 1 }

loadImageIO :: FilePath -> IO Picture
loadImageIO path = do
    fullPath <- getDataFileName ("game/images/" ++ path)
    mPic <- loadJuicy fullPath
    case mPic of
        Nothing -> error $ "Failed to load image: " ++ fullPath
        Just pic -> return pic

renderWrappedText :: Float -> Float -> String -> Picture
renderWrappedText x y s =
    Pictures $ zipWith renderLine [0..] (lines s)
  where
    textScale = 0.15
    lineSpacing = 30
    renderLine i line =
        translate x (y - fromIntegral i * lineSpacing) $
        scale textScale textScale $
        color white $
        text line

render :: GameState -> IO Picture
render gs = return $ Pictures
    [ scale (scaleX * 1.3) (scaleY * 1.3) $ translate 0 60 $ currentBackground gs
    , currentCharacter gs
    , dialogOverlay
    ]
  where
    baseWidth  = 800
    baseHeight = 600
    winW = winWidth gs
    winH = winHeight gs
    scaleX = winW / baseWidth
    scaleY = winH / baseHeight
    blackBar = color black $ rectangleSolid (winW * 2) 200
    wrappedDialog = if null (currentDialog gs)
                    then "No dialogue."
                    else wrapText 700 (currentDialog gs)
    dialoguePic = renderWrappedText (-80) 50 wrappedDialog
    dialogOverlay = translate (-280) (-260) $ Pictures [blackBar, dialoguePic]

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = processNext gs
handleInput (EventKey (MouseButton LeftButton) Down _ _) gs = processNext gs
handleInput (EventResize (newWidth, newHeight)) gs =
    return gs { winWidth = fromIntegral newWidth, winHeight = fromIntegral newHeight }
handleInput _ gs = return gs

updateState :: Float -> GameState -> IO GameState
updateState _ gs = return gs

main :: IO ()
main = do
    scriptPath <- getDataFileName "game/script.chesed"
    scriptContent <- readFile scriptPath
    case parse parseScript scriptPath scriptContent of
        Left err -> putStrLn (errorBundlePretty err) >> exitFailure
        Right cmds -> do
            state <- initialGameState cmds
            playIO window white 60 state render handleInput updateState
  where
    window = InWindow "Chesed" (800,600) (100,100)
