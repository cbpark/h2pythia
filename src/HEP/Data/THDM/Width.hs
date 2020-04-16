module HEP.Data.THDM.Width where

import HEP.Data.THDM                    (BRH2 (..), InputParam, paramToArgs)
import HEP.Data.THDM.Parser             (parseBRH2)

import Control.Monad.Trans.Reader       (ReaderT, ask)
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8            (pack)
import Pipes
import System.Process                   (readProcessWithExitCode)

runh2decays :: MonadIO m
             => FilePath -> Producer (Maybe BRH2) (ReaderT InputParam m) ()
runh2decays h2decaysExe = do
    inputArgs <- paramToArgs <$> lift ask
    (_, brs0, _) <- liftIO (readProcessWithExitCode h2decaysExe inputArgs "")
    case parseOnly parseBRH2 (pack brs0) of
        Left _         -> do liftIO . putStrLn $ "---- error from h2decays!"
                             yield Nothing
        Right (_, brs) -> yield (Just brs)

getWidth :: Monad m => Pipe (Maybe BRH2) (Maybe Double) (ReaderT InputParam m) ()
getWidth = do
    brs0 <- await
    yield $ case brs0 of
                Nothing  -> Nothing
                Just brs -> Just (_totalWidth brs)
