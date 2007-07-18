import Subversion
import Subversion.Hash
import Subversion.Repository
import Prelude hiding (lookup)

main = withSubversion $
       do repos <- openRepository "repos"
          hash  <- new
          update hash "aaa" "bbb"
          lookup hash "aaa" >>= print
          lookup hash "AAA" >>= print
          delete hash "aaa"
          lookup hash "aaa" >>= print
          print "Hello"
