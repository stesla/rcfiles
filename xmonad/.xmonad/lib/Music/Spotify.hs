module Music.Spotify
       (spotifyPlayPause
       ,spotifyNext
       ,spotifyPrev
       ) where

import XMonad
import System.Process


spotifyCmd :: String -> IO ()
spotifyCmd cmd = createProcess (proc "dbus-send" args) >> return ()
  where args = ["--print-reply"
               ,"--dest=org.mpris.MediaPlayer2.spotify"
               ,"/org/mpris/MediaPlayer2"
               ,"org.mpris.MediaPlayer2.Player." ++ cmd
               ]

spotifyPlayPause = io $ (spotifyCmd "PlayPause") :: X ()
spotifyNext = io $ (spotifyCmd "Next") :: X ()
spotifyPrev = io $ (spotifyCmd "Previous") :: X ()
