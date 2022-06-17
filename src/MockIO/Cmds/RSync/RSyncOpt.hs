{-| Interface to the @rsync@ cmdline utility. -}
module MockIO.Cmds.RSync.RSyncOpt
  ( RSyncDeleteOpt(..), RSyncOpt(..), rsyncArchiveOpts, rsyncOpts )
where

import Base1T

-- containers --------------------------

import qualified  Data.Set  as  Set

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.Cmds.RSync.RSyncUsageError  ( AsRSyncUsageError, throwRSyncUsage )

--------------------------------------------------------------------------------

{-| Exclusive choice between types of @rsync --delete-x@.  We don't include
    an equivalent of @rsync --delete@ here; since that behaves differently
    depending on the rsync version on the receiving side.
 -}
data RSyncDeleteOpt = RSYNC_DELETE_BEFORE | RSYNC_DELETE_DURING
                    | RSYNC_DELETE_DELAY | RSYNC_DELETE_AFTER
  deriving (Eq,Ord,Show)

{-| Options to pass to rsync -}
data RSyncOpt = RSYNC_RECURSIVE | RSYNC_PRESERVE_SYMLINKS | RSYNC_PRESERVE_PERMS
              | RSYNC_PRESERVE_MOD_TIMES
              | RSYNC_PRESERVE_GROUP | RSYNC_PRESERVE_OWNER
              | RSYNC_PRESERVE_DEVICE_FILES | RSYNC_PRESERVE_SPECIAL_FILES
              | RSYNC_VERBOSE | RSYNC_PRESERVE_PARTIAL_TRANSFERS
              | RSYNC_PRESERVE_HARD_LINKS
              | RSYNC_NO_CROSS_FS_BOUNDARY | RSYNC_DELETE RSyncDeleteOpt
              | RSYNC_PRESERVE_EXTENDED_ATTRS | RSYNC_PRESERVE_ACLS
  deriving (Eq,Ord,Show)

rsyncOptText ∷ RSyncOpt → 𝕋
rsyncOptText RSYNC_RECURSIVE                    = "--recursive"
rsyncOptText RSYNC_PRESERVE_SYMLINKS            = "--links"
rsyncOptText RSYNC_PRESERVE_PERMS               = "--perms"
rsyncOptText RSYNC_PRESERVE_MOD_TIMES           = "--times"
rsyncOptText RSYNC_PRESERVE_GROUP               = "--group"
rsyncOptText RSYNC_PRESERVE_OWNER               = "--owner"
rsyncOptText RSYNC_PRESERVE_DEVICE_FILES        = "--devices"
rsyncOptText RSYNC_PRESERVE_SPECIAL_FILES       = "--specials"
rsyncOptText RSYNC_PRESERVE_ACLS                = "--acls"
rsyncOptText RSYNC_PRESERVE_EXTENDED_ATTRS      = "--xattrs"
rsyncOptText RSYNC_VERBOSE                      = "--verbose"
rsyncOptText RSYNC_PRESERVE_PARTIAL_TRANSFERS   = "--partial"
rsyncOptText RSYNC_PRESERVE_HARD_LINKS          = "--hard-links"
rsyncOptText RSYNC_NO_CROSS_FS_BOUNDARY         = "--one-file-system"
rsyncOptText (RSYNC_DELETE RSYNC_DELETE_BEFORE) = "--delete-before"
rsyncOptText (RSYNC_DELETE RSYNC_DELETE_DURING) = "--delete-during"
rsyncOptText (RSYNC_DELETE RSYNC_DELETE_DELAY)  = "--delete-delay"
rsyncOptText (RSYNC_DELETE RSYNC_DELETE_AFTER)  = "--delete-after"

{- | A set of flags used during options processing to, e.g., flag up that
     we've already seen a (possibly conflicting) delete option. -}
data RSyncOptFlags = RSyncOptFlags { _delete_type ∷ 𝕄 RSyncDeleteOpt }

----------------------------------------

rsyncOpts' ∷ ∀ ε η . (AsRSyncUsageError ε, MonadError ε η) ⇒
             RSyncOptFlags → RSyncOpt → η (RSyncOptFlags,[𝕋])

rsyncOpts' fs@(RSyncOptFlags { _delete_type = 𝕹 }) o@(RSYNC_DELETE d) =
  return (fs { _delete_type = 𝕵 d },[rsyncOptText o])
rsyncOpts' fs@(RSyncOptFlags { _delete_type = 𝕵 d }) (RSYNC_DELETE d') =
  if d ≡ d'
  then return (fs,[]) -- already handled
  else throwRSyncUsage$[fmtT|conflicting delete types requested: %w vs %w|] d d'
rsyncOpts' fs o = return (fs,[rsyncOptText o])

--------------------

rsyncOpts ∷ ∀ ε η . (AsRSyncUsageError ε, MonadError ε η) ⇒
            Set.Set RSyncOpt → η [𝕋]
rsyncOpts opts =
  let go' (b,xs) a = rsyncOpts' b a ≫ \ (fs',xs') → return (fs',xs ⊕ xs')
      go fs os = foldM go' (fs,[]) os
   in snd ⊳ go (RSyncOptFlags 𝕹) (Set.toList opts)

----------------------------------------

rsyncArchiveOpts ∷ Set.Set RSyncOpt
rsyncArchiveOpts = Set.fromList [ RSYNC_RECURSIVE, RSYNC_PRESERVE_SYMLINKS
                                , RSYNC_PRESERVE_PERMS, RSYNC_PRESERVE_MOD_TIMES
                                , RSYNC_PRESERVE_GROUP, RSYNC_PRESERVE_OWNER
                                , RSYNC_PRESERVE_DEVICE_FILES
                                , RSYNC_PRESERVE_SPECIAL_FILES
                                ]


-- that's all, folks! ----------------------------------------------------------
