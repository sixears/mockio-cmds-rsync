{-| Interface to the @rsync@ cmdline utility. -}
module MockIO.Cmds.RSync.RSync
  ( -- re-exports for user convenience
    AsRSyncUsageError(..), RSyncDeleteOpt(..), RSyncOpt(..), RSyncUsageError
  , rsync, rsync', rsyncArchive, rsyncArchiveOpts, rsyncArgs, rsyncOpts
  )
where

import Base1T

-- containers --------------------------

import qualified  Data.Set  as  Set

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity )

-- mockio ------------------------------

import MockIO.DoMock  ( HasDoMock( doMock ) )

-- mockio-log --------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Process  ( ꙩ )

-- monadio-plus ------------------------

import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, asks )

-- safe --------------------------------

import Safe  ( succSafe )

-- stdmain -----------------------------

import StdMain.ProcOutputParseError ( AsProcOutputParseError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified MockIO.Cmds.RSync.Paths  as  Paths
import MockIO.Cmds.RSync.RSyncOpt        ( RSyncDeleteOpt(..), RSyncOpt(..)
                                         , rsyncArchiveOpts,rsyncOpts )
import MockIO.Cmds.RSync.RSyncUsageError ( AsRSyncUsageError(..)
                                         , RSyncUsageError )
import MockIO.Cmds.RSync.ToRSyncArgs     ( ToRSyncArgs, rsyncArgs, logRSyncMsg )

--------------------------------------------------------------------------------

{-| Run an @rsync@.  The args are user-provided, and not checked.  No basic
    rsync message is emitted. -}
rsync' ∷ ∀ ε ρ μ .
        (MonadIO μ, HasDoMock ρ, MonadReader ρ μ,
         AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
         AsFPathError ε, AsIOError ε, Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log MockIOClass) μ) ⇒
        Severity → [𝕋] → μ ()
rsync' sev full_args = snd ⊳ ꙩ (Paths.rsync, full_args, succSafe sev)

--------------------

{-| Run an @rsync@. A basic rsync message is emitted; further cmd messages
    are made at @sev+1@. -}
rsync ∷ ∀ ε α ρ μ .
        (MonadIO μ, ToRSyncArgs α, HasDoMock ρ, MonadReader ρ μ,
         AsProcOutputParseError ε, AsProcExitError ε, AsCreateProcError ε,
         AsFPathError ε, AsIOError ε, AsRSyncUsageError ε,
         Printable ε, MonadError ε μ, HasCallStack,
         MonadLog (Log MockIOClass) μ) ⇒
        Severity → Set.Set RSyncOpt → α → μ ()
rsync sev opts args = do
  do_mock ← asks (view doMock)
  logRSyncMsg sev args do_mock
  rsyncArgs opts args ≫ rsync' sev

----------------------------------------

{-| Run an @rsync@, with the explicit equivalent of @--archive@ in addition to
    the given options.  This throws back any `RSyncUsageError` independently
    to allow it to be munged. -}
rsyncArchive ∷ ∀ ε' ε α ρ η μ .
               (MonadIO μ, ToRSyncArgs α, HasDoMock ρ, MonadReader ρ μ,
                AsProcOutputParseError ε, AsProcExitError ε,AsCreateProcError ε,
                AsFPathError ε, AsIOError ε,
                Printable ε, MonadError ε μ, HasCallStack,
                AsRSyncUsageError ε', Printable ε', MonadError ε' η,
                MonadLog (Log MockIOClass) μ) ⇒
               Severity → Set.Set RSyncOpt → α → η (μ ())
rsyncArchive sev opts args =
  let rsync_opts = Set.union rsyncArchiveOpts opts
   in case rsyncArgs rsync_opts args of
        𝕷 e  → throwError e
        𝕽 as → return $ do
          do_mock ← asks (view doMock)
          logRSyncMsg sev args do_mock
          rsync' sev as

-- that's all, folks! ----------------------------------------------------------
