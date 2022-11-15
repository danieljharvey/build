module Main where

import Data.Text (Text)

-- unique id for a jobby
newtype TaskKey = TaskKey Text

data Task = Task
  { taskKey :: TaskKey,
    dependsOn :: [TaskKey]
  }

-- take a container name, run it, returning port
withDocker :: Text -> (Word16 -> IO b) -> IO b
withDocker path useDocker =
  bracket
    (setupDocker path)
    (\dockerId -> stopDocker dockerId)
    (\dockerId -> useDocker dockerId)

main :: IO ()
main = putStrLn "dog"
