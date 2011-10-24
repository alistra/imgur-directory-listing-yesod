import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withImgurDirectoryListing)

main :: IO ()
main = defaultMain fromArgs withImgurDirectoryListing