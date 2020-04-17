module HEP.Data.THDM.Width (printWidthH2, printWidthHp) where

import HEP.Data.THDM
import HEP.Data.THDM.Parser             (parseBRH2, parseBRHp)

import Control.Monad.Trans.Reader       (ReaderT, ask)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Data.ByteString.Char8            (pack)
import Pipes
import Pipes.Lift                       (runReaderP)
import System.Process                   (readProcessWithExitCode)

import Control.Monad                    (forever)
import System.IO                        (Handle, hPutStrLn)

runDecays :: MonadIO m
          => (InputParam -> [String])
          -> Parser (InputParam, a)
          -> FilePath
          -> Producer (Maybe a) (ReaderT InputParam m) ()
runDecays paramToArgs parser exe = do
    inputArgs <- paramToArgs <$> lift ask
    (_, brs0, _) <- liftIO (readProcessWithExitCode exe inputArgs "")
    case parseOnly parser (pack brs0) of
        Left _         -> do liftIO . putStrLn $ "---- error from " ++ exe ++ "!"
                             yield Nothing
        Right (_, brs) -> yield (Just brs)

runh2decays :: MonadIO m
            => FilePath -> Producer (Maybe BRH2) (ReaderT InputParam m) ()
runh2decays = runDecays paramToArgsH2 parseBRH2

runhpdecays :: MonadIO m
            => FilePath -> Producer (Maybe BRHp) (ReaderT InputParam m) ()
runhpdecays = runDecays paramToArgsHp parseBRHp

getWidth :: Monad m
         => (a -> Double)
         -> Pipe (Maybe a) (Maybe Double) (ReaderT InputParam m) ()
getWidth totalWidthOf = do
    brs0 <- await
    yield $ case brs0 of
                Nothing  -> Nothing
                Just brs -> Just (totalWidthOf brs)

getWidthH2 :: Monad m
           => Pipe (Maybe BRH2) (Maybe Double) (ReaderT InputParam m) ()
getWidthH2 = getWidth _totalWidthH2

getWidthHp :: Monad m
           => Pipe (Maybe BRHp) (Maybe Double) (ReaderT InputParam m) ()
getWidthHp = getWidth _totalWidthHp

printWidthH2 :: MonadIO m => Handle -> FilePath -> InputParam -> m ()
printWidthH2 h h2decaysExec inp = runEffect $
    runReaderP inp (runh2decays h2decaysExec >-> getWidthH2)
    >-> printWidth' h "35:mWidth = "

printWidthHp :: MonadIO m => Handle -> FilePath -> InputParam -> m ()
printWidthHp h hpdecaysExec inp = runEffect $
    runReaderP inp (runhpdecays hpdecaysExec >-> getWidthHp)
    >-> printWidth' h "37:mWidth = "

printWidth' :: MonadIO m => Handle -> String -> Consumer (Maybe Double) m ()
printWidth' h idstr = forever $ do
    w0 <- await
    case w0 of
        Nothing -> return ()
        Just w  -> liftIO . hPutStrLn h $ idstr <> show w
