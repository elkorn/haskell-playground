module UI.Terminal where

import System.Console.ANSI
import System.IO
import qualified Data.Map as M

import Types
import Level

-- import UI.Types
-- instance UI TerminalUI where
--   prepareGame _ = do
--     hSetEcho stdin False
--     hSetBuffering stdin NoBuffering
--     hSetBuffering stdout NoBuffering
--     hideCursor
--     setTitle "Game!"
--   drawCharacter _ (x, y) = do
--     clearScreen
--     setCursorPosition y x
--     setSGR [ SetConsoleIntensity BoldIntensity
--             , SetColor Foreground Vivid Blue ]
--     putStr "@"
--   handleExit _ = do
--     setSGR [ Reset ]
--     clearScreen
--     setCursorPosition 0 0
--     showCursor
--     putStrLn "Goodbye!"

sgrData :: M.Map Char [SGR]
sgrData = M.fromList
        [
          ( '@'
          , [ SetConsoleIntensity BoldIntensity
            , SetColor Foreground Vivid Blue
            ])
        , ( '#'
          , [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Black ])
        , ( '!'
          , [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Magenta])
        , ( 'v'
          , [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red])
        , ( ')'
          , [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Cyan])
        , ( '>'
          , [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Blue])
        , ( '<'
          , [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Dull Cyan])
        , ( '+'
          , [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Magenta])
        , ( '-'
          , [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Dull Yellow])
        , ( '~'
          , [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid Green])
        , ( '.'
          , [ Reset ])
        , ( '?'
          , [ Reset ])
        ]

getSgr = flip M.lookup $ sgrData

blah =
  setCursorPosition 41 0

prepareGame :: WorldState -> IO ()
prepareGame world = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hideCursor
    setTitle "Game!"
    -- clearScreen
    drawWorld world

drawHero :: WorldState -> IO ()
drawHero world
  | newPosition == oldPosition = return ()
  | otherwise = do
        drawCharacter newPosition world
        drawCharacter oldPosition world
  where
    hero = worldHero world
    newPosition = heroPosition hero
    oldPosition = heroOldPosition hero

drawWorld :: WorldState -> IO ()
drawWorld world = do
  setCursorPosition 0 0
  let level = worldLevel world
  let (maxX, maxY) = levelMax level
  let coordinates = foldl (++) [] [[ (x,y) | x <- [0..maxX]] | y <- [0..maxY]]
  mapM_ (flip drawCharacter $ world) $ coordinates

drawCharacter :: (Int, Int) -> WorldState -> IO ()
drawCharacter (x,y) world = do
    let ch = coordinatesToCharacter (x,y) world
    let tile = M.lookup (x,y) $ (levelTiles . worldLevel) world
    -- putStrLn $ show (x,y) ++ " " ++ show tile ++ " " ++ show ch
    setCursorPosition y x
    case getSgr ch of
      Nothing -> do
        -- putChar 'X'
        return ()
      Just sgr -> do
        setSGR sgr
        putChar ch

handleExit :: IO ()
handleExit = do
    setSGR [Reset]
    clearScreen
    setCursorPosition 0 0
    showCursor
    putStrLn "Goodbye!"

coordinatesToCharacter :: Coordinates -> WorldState -> Char
coordinatesToCharacter coordinates (World hero _ level _)
  | coordinates == heroPosition hero = '@'
  | isAcid        coordinates level  = '~'
  | isClosedDoor  coordinates level  = '+'
  | isOpenDoor    coordinates level  = '-'
  | isDownstairs  coordinates level  = '<'
  | isGold        coordinates level  = '$'
  | isPotion      coordinates level  = '!'
  | isUpstairs    coordinates level  = '>'
  | isMonster     coordinates level  = 'v'
  | isWall        coordinates level  = '#'
  | isWeapon      coordinates level  = ')'
  | isFloor       coordinates level  = ' '
  | otherwise                        = '?'
