import qualified Data.ByteString.Lazy as Lazy
import Subversion
import Subversion.Repository
import Subversion.FileSystem
import Subversion.FileSystem.Revision
import Subversion.FileSystem.Root
import Subversion.FileSystem.Transaction

main = withSubversion $
       do putStrLn "Creating a repository \"repos\"..."
          repos  <- createRepository "repos" [] []
          fs     <- getRepositoryFS repos


          rev    <- getYoungestRev fs
          putStrLn ("The youngest revision of the repository is " ++ show rev)


          putStrLn "Storing a file \"/hello\" in it..."
          Right newRev <- doReposTxn repos rev "HelloWorld" (Just "New file: /hello")
                          $ do makeFile "/hello"
                               applyText "/hello" Nothing "Hello, world!"
          putStrLn ("Succeeded. The revision is " ++ show newRev ++ ".")


          putStrLn ("Getting the content of \"/hello\" at revision " ++ show newRev ++ "...")
          content <- withRevision fs newRev
                     $ getFileContents "/hello"
          putStrLn ("-------------\n" ++ content ++ "\n-------------")


          putStrLn "Writing the whole content of repository to `repository.dump'..."
          dumpRepository repos Nothing Nothing True True >>= Lazy.writeFile "repository.dump"
          

          putStrLn "Deleting the repository..."
          deleteRepository "repos"
