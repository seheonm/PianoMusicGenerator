import Yesod.Static (static)
import Application () -- for YesodDispatch instance
import Foundation
import Yesod.Core

main :: IO ()
main = do
    staticMgr <- static "static"  -- Initialize the static file server
    warp 3000 App { getStatic = staticMgr }