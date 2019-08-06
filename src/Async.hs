module Async where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Say

talker :: String -> IO ()
talker str = forever $ do
  sayString str
  threadDelay 500000

getResult :: IO Int
getResult = do
  sayString "Doing some big computation..."
  threadDelay 2000000
  sayString "Done!"
  return 42

main :: IO ()
main = do
  async1 <- async $ talker "async"
  withAsync (talker "withAsync") $ \async2 -> do
    async3 <- async getResult

    res <- poll async3
    case res of
      Nothing -> sayString "getResult still running"
      Just (Left e) -> sayString $ "getResult failed: " ++ show e
      Just (Right x) -> sayString $ "getResult finished: " ++ show x

    res <- waitCatch async3
    case res of
      Left e -> sayString $ "getResult failed: " ++ show e
      Right x -> sayString $ "getResult finished: " ++ show x

    res <- wait async3
    sayString $ "getResult finished: " ++ show (res :: Int)

  sayString "withAsync talker should be dead, but not async"
  threadDelay 2000000

  sayString "Now killing async talker"
  cancel async1

  threadDelay 2000000
  sayString "Goodbye!"