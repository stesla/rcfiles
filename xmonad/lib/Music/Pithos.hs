module Music.Pithos (
       pithosSelect
       ) where

import qualified Data.Map as M
import XMonad
import XMonad.Actions.GridSelect
import System.Cmd

pithosNavigation :: TwoD a (Maybe a)
pithosNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [
          ((0,xK_Escape), cancel)
         ,((0,xK_Return), select)
         ,((0,xK_Left)  , move (-1,0)  >> select)
         ,((0,xK_Right) , move (1,0)   >> select)
         ,((0,xK_Down)  , move (0,1)   >> select)
         ,((0,xK_Up)    , move (0,-1)  >> select)
         ,((0,xK_space) , setPos (0,0) >> select)
         ]
        navDefaultHandler = const pithosNavigation

pithosGSConfig = defaultGSConfig {
      gs_navigate   = pithosNavigation
    , gs_cellheight = 30
    , gs_cellwidth  = 100
}

pithosCmd :: String -> IO ()
pithosCmd cmd = rawSystem "dbus-send" args >> return ()
  where args = ["--print-reply"
               ,"--dest=net.kevinmehall.Pithos"
               ,"/net/kevinmehall/Pithos"
               ,"net.kevinmehall.Pithos." ++ cmd
               ]

-- PlayPause, SkipSong, LoveCurrentSong, BanCurrentSong,
-- TiredCurrentSong, Present (raises the window), GetCurrentSong,
-- IsPlaying
stringList = [("Play/Pause", io $ (pithosCmd "PlayPause"))
             ,("Ban", io $ (pithosCmd "BanCurrentSong"))
             ,("Skip", io $ (pithosCmd "SkipSong"))
             ,("Love", io $ (pithosCmd "LoveCurrentSong"))
             ,("Tired", io $ (pithosCmd "TiredCurrentSong"))]

pithosSelect = runSelectedAction pithosGSConfig stringList
