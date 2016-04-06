module Helper.HttpExceptionHandler
    ( handleHttpException
    ) where

import Import

import Network.HTTP.Client ()

handleHttpException :: (Either HttpException ()) -> Handler ()
handleHttpException (Left e) = printHttpException e
handleHttpException _ = return ()

printHttpException :: MonadIO m => HttpException -> m ()
printHttpException (StatusCodeException status headers _) = putStrLn $
    "!!! Error! Status code "
    <> code
    <> " "
    <> errors
    where
        errors = tshow $ lookup "X-Response-Body-Start" headers
        code = tshow $ statusCode status

printHttpException e = putStrLn $ "!!! Error! (not status code) " <> tshow e
