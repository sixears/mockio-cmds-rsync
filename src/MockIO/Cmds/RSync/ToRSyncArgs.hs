{-| Interface to the @rsync@ cmdline utility. -}
module MockIO.Cmds.RSync.ToRSyncArgs
  ( ToRSyncArgs(..), logRSyncMsg, rsyncArgs )
where

import Base1T

-- containers --------------------------

import qualified  Data.Set  as  Set

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( HasDoMock, DoMock )

-- mockio-log --------------------------

import MockIO.IOClass      ( HasIOClass )
import MockIO.Log          ( logio )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Cmds.RSync.RSyncOpt        ( RSyncOpt, rsyncOpts )
import MockIO.Cmds.RSync.RSyncUsageError ( AsRSyncUsageError )

--------------------------------------------------------------------------------

{-| Possible ways to request a rsync -}
class ToRSyncArgs α where
  {-| Convert to arguments to @rsync@ -}
  toRSyncArgs ∷ α → [𝕋]
  {-| Write a msg to the log that we're about to rsync -}
  rsyncMsg ∷ α → 𝕋

logRSyncMsg ∷ ∀ α ω μ .
              (MonadIO μ, ToRSyncArgs α,
               Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
              Severity → α → DoMock → μ ()
logRSyncMsg sev a do_mock = logio sev (rsyncMsg a) do_mock

{-| rsync one directory to another; each at the same level (so they will be
    passed to rsync as @from-dir/@ @to-dir/@; the contents of to-dir will be the
    copied contents of from-dir. -}
instance ToRSyncArgs (AbsDir, AbsDir) where
  toRSyncArgs (f,d) = [toText f, toText d]
  rsyncMsg (f,d) = [fmtT|rsyncing %T to %T|] f d

rsyncArgs ∷ ∀ ε α η . (ToRSyncArgs α, AsRSyncUsageError ε, Printable ε,
                       MonadError ε η, HasCallStack) ⇒
            Set.Set RSyncOpt → α → η [𝕋]
rsyncArgs os as = do
  opts ← rsyncOpts os
  return $ opts ⊕ toRSyncArgs as
-- that's all, folks! ----------------------------------------------------------
