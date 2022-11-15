module Main where

import Data.Text (Text)

-- unique id for a jobby
newtype TaskKey = TaskKey Text

data Task = Task
  { taskKey :: TaskKey,
    dependsOn :: [TaskKey]
  }

main :: IO ()
main = putStrLn "dog"
