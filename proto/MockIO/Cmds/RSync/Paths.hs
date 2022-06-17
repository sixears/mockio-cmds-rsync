{-# LANGUAGE QuasiQuotes #-}

{-| Filesystem paths to external programs; to be filled in at build time. -}
module MockIO.Cmds.RSync.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

{-| Path to @rsync@ -}
rsync :: AbsFile
rsync = [absfile|__rsync__/bin/rsync|]
