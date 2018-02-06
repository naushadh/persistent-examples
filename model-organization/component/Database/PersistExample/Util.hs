module Database.PersistExample.Util (persistManyFileWith) where

import qualified System.IO as SIO
import qualified Data.Text.IO as TIO
import qualified Database.Persist.Quasi as PQ
import qualified Database.Persist.TH as PTH
import           Language.Haskell.TH.Syntax (Q, Exp, qRunIO)

-- | Same as 'persistWith', but uses an external file instead of a
-- quasiquotation, and works on _many_ files.
persistManyFileWith :: PQ.PersistSettings -> [FilePath] -> Q Exp
persistManyFileWith ps fps = do
    ss <- mapM getS fps
    let s = mconcat ss
    PTH.parseReferences ps s
  where
    getS fp = do
      h <- qRunIO $ SIO.openFile fp SIO.ReadMode
      qRunIO $ SIO.hSetEncoding h SIO.utf8_bom
      s <- qRunIO $ TIO.hGetContents h
      return s