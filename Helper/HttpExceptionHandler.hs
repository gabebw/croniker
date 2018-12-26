module Helper.HttpExceptionHandler
    ( handleHttpException
    ) where

import Import

import Network.HTTP.Client ()

handleHttpException :: Either HttpException () -> Handler ()
handleHttpException (Left (HttpExceptionRequest _ content)) = printHttpException content
handleHttpException _ = return ()

printHttpException :: MonadIO m => HttpExceptionContent -> m ()
printHttpException (StatusCodeException response _) = putStrLn $
    "!!! Error! Status code "
    <> code
    <> " "
    <> errors
    where
        status = responseStatus response
        headers = responseHeaders response
        errors = tshow $ lookup "X-Response-Body-Start" headers
        code = tshow $ statusCode status

printHttpException e = putStrLn $ "!!! Error! (not status code) " <> tshow e
