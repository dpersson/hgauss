{-# LANGUAGE OverloadedStrings #-}
module Retry where

import Control.Concurrent (forkIO, threadDelay, readChan, writeChan, newChan)
import Control.Concurrent.STM (atomically, writeTChan, readTChan, newTChan)
import Control.Concurrent.STM.TBMChan (readTBMChan, writeTBMChan, newTBMChan, closeTBMChan)
import Control.Monad (replicateM_, when)
import System.Random (randomRIO)

{-
https://www.fpcomplete.com/blog/2016/11/comparative-concurrency-with-haskell
https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-software-transactional-memory
-}

workerCount = 250
workloadCount = 10000
minDelay = 250000 -- in microseconds, == 0.25 seconds
maxDelay = 750000 --                  == 0.75 seconds

worker requestChan responseChan workerId = forkIO $ do
    let loop = do
            delay <- randomRIO (minDelay, maxDelay)
            threadDelay delay

            -- Interact with the STM channels atomically
            toContinue <- atomically $ do
                -- Get the next request, if the channel is open
                mint <- readTBMChan requestChan
                case mint of
                    -- Channel is closed, do not continue
                    Nothing -> return False
                    -- Channel is open and we have a request
                    Just int -> do
                        -- Write the response to the response channel
                        writeTChan responseChan (workerId, int, int * int)
                        -- And yes, please continue
                        return True
            when toContinue loop

    -- Kick it off!
    loop

retry = do
    -- Create our communication channels. We're going to ensure the
    -- request channel never gets more than twice the size of the
    -- number of workers to avoid high memory usage.
    requestChan <- atomically $ newTBMChan (workerCount * 2)
    responseChan <- atomically newTChan

    mapM_ (worker requestChan responseChan) [1..workerCount]

    -- Fill up the request channel in a dedicated thread
    forkIO $ do
        mapM_ (atomically . writeTBMChan requestChan) [1..workloadCount]
        atomically $ closeTBMChan requestChan

    replicateM_ workloadCount $ do
        -- Read the result off of the response channel
        (workerId, int, square) <- atomically $ readTChan responseChan
        -- Print out a little message
        putStrLn $ concat
            [ "Worker #"
            , show workerId
            , ": square of "
            , show int
            , " is "
            , show square
            ]