import Subversion
import Subversion.FileSystem
import Subversion.FileSystem.Root
import Subversion.FileSystem.Transaction
import Subversion.Hash
import Subversion.Repository
import Subversion.Stream
import Prelude hiding (lookup)

main = withSubversion $
       do repos <- openRepository "repos"
          fs    <- getRepositoryFS repos
          rev   <- getYoungestRev fs

          doReposTxn repos rev "PHO" "txn test"
                         $ makeFile "/hello?"

{-
          hello <- withRevision fs rev $
                   getFileContents "/hello"
          putStr hello
-}

{-
          root  <- getRevisionRoot fs rev
          getDirEntries root "/" >>= print
-}

{-
          txn   <- beginTxnForCommit repos rev "PHO" "txn test"
          root  <- getTransactionRoot txn
-}


--          makeFile root "/tmp_"
--          abortTxn txn
--          commitTxn repos txn >>= print

          return ()
