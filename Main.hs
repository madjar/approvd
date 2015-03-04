import Web
import Yesod

main :: IO ()
main = warp 3000 =<< makeFoundation
