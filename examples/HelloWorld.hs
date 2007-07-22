import Subversion
import Subversion.FileSystem
import Subversion.Hash
import Subversion.Repository
import Subversion.Stream
import Prelude hiding (lookup)

main = withSubversion $
       do repos <- openRepository "repos"
          fs    <- getRepositoryFS repos
          rev   <- getYoungestRev fs

          txn   <- beginTxnForCommit repos rev "PHO" "txn test"
          root  <- getTransactionRoot txn

          applyText root "/hello" Nothing "Hello, world!\x0a"
          abortTxn txn
          -- commitTxn repos txn >>= print

          return ()
