{-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE DeriveFunctor #-}
module Library (main) where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Free
import Data.Text (Text)
import Debug.Trace
import qualified System.Command as Cmd
import System.Exit

fromExitCode :: ExitCode -> ShellOutput
fromExitCode ec =
  ShellOutput ec mempty mempty

data ShellOutput = ShellOutput
  { soExitCode :: ExitCode,
    soStdout :: String,
    soStderr :: String
  }
  deriving (Eq, Ord, Show)

data MakeF next
  = Shell String [String] (Maybe String) (ShellOutput -> next)
  | Print String next
  | Exit ExitCode (ShellOutput -> next)

instance Functor MakeF where
  fmap f (Shell s opts stdin next) = Shell s opts stdin (f . next)
  fmap f (Print s next) = Print s (f next)
  fmap f (Exit exitCode next) = Exit exitCode (f . next)

data ExitF next
  = ExitNew ExitCode (ExitCode -> next)
  deriving Functor

data Sum f g a
    = InL (f a)
    | InR (g a)
    deriving Functor

type Make = Free (Sum ExitF MakeF)

makeCurl :: String -> Make ShellOutput
makeCurl url = liftF $ InR (Shell "curl" [url, "-m", "2"] Nothing id)

makeDocker :: [String] -> Make ShellOutput
makeDocker opts =
  liftF $ InR (Shell "docker" opts Nothing id)

makeSed :: String -> String -> Make ShellOutput
makeSed pat stdin = liftF $ InR (Shell "sed" ["-E", pat] (Just stdin) id)

makePrint :: String -> Make ()
makePrint str = liftF $ InR (Print str ())

makeExit :: ExitCode -> Make ShellOutput
makeExit exitCode = liftF $ InR (Exit exitCode id)

postgresHealthCheck :: PostgresInstance -> Make ShellOutput
postgresHealthCheck postgresInstance@(PostgresInstance {pgPort}) = do
  output <- liftF $ InR (Shell "psql" opts Nothing id)
  case soExitCode output of
    ExitSuccess ->
      makePrint ("Postgres is ready at " <> show pgPort) 
        >> makeExit ExitSuccess
    ExitFailure _ ->
      makePrint (soStderr output)
        >> makePrint (soStdout output)
        >> makePrint ("Could not reach postgres at " <> show pgPort)
        >> makeExit (ExitFailure 1)
  where
    opts =
      [ postgresConnectionString postgresInstance,
          "--list" -- list all dbs as healthcheck
      ]

data PostgresInstance = PostgresInstance
  { pgPort :: Word,
    pgDockerName :: String,
    pgPassword :: String
  }

postgresConnectionString :: PostgresInstance -> String
postgresConnectionString (PostgresInstance {pgPort, pgPassword}) =
  "postgresql://postgres:" <> pgPassword <> "@localhost:" <> show pgPort

-- | there are generalised Docker commands to be had here,
-- let's factor them out as we use them though
runPostgres :: String -> String -> Make PostgresInstance
runPostgres customName password = do
  dockerRun <-
    makeDocker
      [ "run",
        "--name",
        customName,
        "-p",
        "5432",
        "-e",
        "POSTGRES_PASSWORD=" <> password,
        "-d",
        "postgres"
      ] 

  dockerPort <- makeDocker ["port", customName, "5432"] 

  port <- soStdout <$> makeSed "s/.*://" (soStdout dockerPort)

  let postgresInstance =
        PostgresInstance
          { pgPort = read port,
            pgDockerName = customName,
            pgPassword = password
          }

  postgresHealthCheck postgresInstance
  pure postgresInstance

stopPostgres :: String -> Make ShellOutput
stopPostgres customName = do
  output <- makeDocker ["stop", customName] >> makeDocker ["rm", customName]
  case soExitCode output of
    ExitSuccess ->
      makePrint ("Stopped " <> customName) >> makeExit ExitSuccess
    ExitFailure _ ->
      makePrint (soStderr output)
        >> makePrint (soStdout output)
        >> makePrint ("Could not stop " <> customName)
        >> makeExit (ExitFailure 1)

interpretIO :: Make a -> IO a
interpretIO =
  foldFree interpret
  where
    interpret :: Sum ExitF MakeF a -> IO a
    interpret prog' =
      case prog' of
        InL _ -> error "dfdfg"
        InR inner -> case inner of
          Shell cmd opts stdin next -> do
            let cmdOptions = case stdin of
                  Just str -> [Cmd.Stdin str]
                  _ -> []
            print (cmd,cmdOptions, opts)
            Cmd.command cmdOptions cmd opts
              >>= \(Cmd.Exit exitCode, Cmd.Stdout stdout, Cmd.Stderr stderr) ->
                pure $ next (ShellOutput exitCode stdout stderr)
          Print str a -> Prelude.putStrLn str >> pure a
          Exit exitCode next -> pure $ next (fromExitCode exitCode)

main :: IO ()
main = void $ interpretIO $ do
  pgInstance <- runPostgres "1mypostgres" "mysecretpassword"
  stopPostgres (pgDockerName pgInstance)
