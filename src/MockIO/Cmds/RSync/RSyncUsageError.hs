{-| Interface to the @rsync@ cmdline utility. -}
module MockIO.Cmds.RSync.RSyncUsageError
  ( AsRSyncUsageError(..), RSyncUsageError, throwRSyncUsage )
where

import Base1T

-- base --------------------------------

import GHC.Generics  ( Generic )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- text-printer ------------------------

import qualified  Text.Printer  as P

--------------------------------------------------------------------------------

data RSyncUsageError = RSyncUsageError { _txt ∷ 𝕋, _callstack ∷ CallStack }
  deriving (Generic,NFData,Show)

----------------------------------------

instance Exception RSyncUsageError

----------------------------------------

instance Eq RSyncUsageError where
  (RSyncUsageError a _) == (RSyncUsageError b _) = a == b

----------------------------------------

instance HasCallstack RSyncUsageError where
  callstack = lens _callstack (\ eu cs → eu { _callstack = cs })

----------------------------------------

class AsRSyncUsageError ε where
  _RSyncUsageError ∷ Prism' ε RSyncUsageError

--------------------

instance AsRSyncUsageError RSyncUsageError where
  _RSyncUsageError = id

--------------------

instance Printable RSyncUsageError where
  print = P.text ∘ _txt

------------------------------------------------------------

rsyncUsageError ∷ ∀ τ ε . (AsRSyncUsageError ε, Printable τ, HasCallStack) ⇒
                  τ → ε
rsyncUsageError t = _RSyncUsageError # RSyncUsageError (toText t) callStack

----------------------------------------

throwRSyncUsage ∷ ∀ τ ε ω η .
                  (Printable τ, AsRSyncUsageError ε, MonadError ε η) ⇒ τ → η ω
throwRSyncUsage t = throwError $ rsyncUsageError t

-- that's all, folks! ----------------------------------------------------------
