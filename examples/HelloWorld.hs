import Subversion
import Subversion.FileSystem
import Subversion.Hash
import Subversion.Repository
import Prelude hiding (lookup)

main = withSubversion $
       do repos <- openRepository "repos"
          fs    <- getRepositoryFS repos
          root  <- getRevisionRoot fs 0
          return ()
