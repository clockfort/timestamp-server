import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import qualified Data.ByteString as B
import Snap.Core
import Snap.Http.Server
import Data.Global
--import Control.Applicative
import Data.Text hiding (map, concat, head, last, zip)

increment :: TVar Int -> STM ()
increment counter = do
	x <- readTVar counter
	writeTVar counter (x + 1)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = ifTop (atomicGetTimestamp)


--atomicGetTimestamp :: Snap ()
atomicGetTimestamp = do
	let timestamp = declareTVar "timestamp" 0
	counter <- liftIO $ atomically (increment timestamp)
	current <- liftIO $ atomically (readTVar timestamp)
	writeText $ pack $ show current
