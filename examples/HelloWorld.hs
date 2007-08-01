import Subversion
import Subversion.FileSystem
import Subversion.FileSystem.Revision
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

{-
          doReposTxn repos rev "PHO" "txn test"
                         $ do hello <- getFileContents "/hello"
                              unsafeIOToFS $ putStrLn hello
                              fail ""
-}

          hello <- withRevision fs rev $
                   getNodeHistory True "/tmp"
          print hello

{-
          root  <- getRevisionRoot fs rev
          getDirEntries root "/" >>= print
-}



--          makeFile root "/tmp_"
--          abortTxn txn
--          commitTxn repos txn >>= print

          return ()
