import Control.Concurrent.STM
import Control.Monad.Trans
import Snap.Core
import Snap.Http.Server
import Data.Global
import Data.Text hiding (map, concat, head, last, zip)

increment :: TVar Int -> STM ()
increment counter = do
	x <- readTVar counter
	writeTVar counter (x + 1)

main = quickHttpServe $ ifTop atomicGetTimestamp

atomicGetTimestamp = do
	let timestamp = declareTVar "timestamp" 0
	counter <- liftIO $ atomically (increment timestamp)
	writeText $ pack $ show counter
