import Control.Concurrent.STM
import Control.Monad.Trans
import Snap.Core
import Snap.Http.Server
import Data.Global
import Data.Text hiding (map, concat, head, last, zip)
import qualified Data.ByteString.Char8 as B

increment :: TVar Int -> STM ()
increment counter = do
	x <- readTVar counter
	writeTVar counter (x + 1)

main = httpServe config $ ifTop atomicGetTimestamp

config = setBind (B.pack "0.0.0.0") . setPort 80 $ defaultConfig

atomicGetTimestamp = do
	let timestamp = declareTVar "timestamp" 0
	counter <- liftIO $ atomically (increment timestamp)
	current <- liftIO $ atomically (readTVar timestamp)
	writeText $ pack $ show current 
