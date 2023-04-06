{-# LANGUAGE NamedFieldPuns #-}
  {-# LANGUAGE LambdaCase #-}
    {-# LANGUAGE DerivingStrategies #-}
module Library (main) where

import Control.Monad.Except
import Control.Monad.Free
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
  deriving stock ( Eq, Ord, Show)

data MakeF next
  = Shell String [String] (Maybe String) (ShellOutput -> next)
  | Print String next
  | Exit ExitCode (ShellOutput -> next)
  | Or (Make next) (ShellOutput -> Make next)

instance Functor MakeF where
  fmap f (Shell s opts stdin next) = Shell s opts stdin (f . next)
  fmap f (Print s next) = Print s (f next)
  fmap f (Exit exitCode next) = Exit exitCode (f . next)
  fmap f (Or comp next) = Or (fmap f comp) (fmap f . next)

type Make = Free MakeF

_makeCurl :: String -> Make ShellOutput
_makeCurl url = liftF  (Shell "curl" [url, "-m", "2"] Nothing id)

makeDocker :: [String] -> Make ShellOutput
makeDocker opts =
  liftF (Shell "docker" opts Nothing id)

makeSed :: String -> String -> Make ShellOutput
makeSed pat stdin = liftF (Shell "sed" ["-E", pat] (Just stdin) id)

makePrint :: String -> Make ()
makePrint str = liftF (Print str ())

makeExit :: ExitCode -> Make ShellOutput
makeExit exitCode = liftF (Exit exitCode id)

makeOr :: Make a -> (ShellOutput -> Make a) -> Make a
makeOr comp f = liftF (Or comp f)

makeTry :: Make a -> Make (Either ShellOutput a)
makeTry comp = makeOr (Right <$> comp) (pure . Left )

postgresHealthCheck :: PostgresInstance -> Make ShellOutput
postgresHealthCheck postgresInstance@(PostgresInstance {pgPort}) = do
  output <- liftF (Shell "psql" opts Nothing id)
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
runPostgres :: String -> String -> Make (Maybe PostgresInstance)
runPostgres customName password = do 
  eitherPg <- makeTry $ do
    _ <- makeTry (stopPostgres customName)
    _ <-
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

    _ <- makeRetry 3 (postgresHealthCheck postgresInstance)

    pure postgresInstance
  case eitherPg of
    Left err -> makePrintErrors err >> pure Nothing
    Right pg -> pure (Just pg)
      
makePrintErrors :: ShellOutput -> Make ()
makePrintErrors (ShellOutput {soStdout,soStderr})
  = makePrint soStdout >> makePrint soStderr

makeSleep :: Int -> Make ()
makeSleep s = 
  liftF (Shell "sleep" [show s] Nothing (const ()))

makeRetry :: Int -> Make a -> Make (Either ShellOutput a)
makeRetry sleep comp = do
  result <- makeTry comp
  case result of
    Right a -> pure (Right a)
    Left _ -> makeSleep sleep >> makeTry comp

stopPostgres :: String -> Make ShellOutput
stopPostgres customName = do
  output <- makeDocker ["stop", customName] >> makeDocker ["rm", customName]
  case soExitCode output of
    ExitSuccess ->
      makePrint ("Stopped " <> customName) >> makeExit ExitSuccess
    ExitFailure _ ->
      makePrintErrors output
        >> makePrint ("Could not stop " <> customName)
        >> makeExit (ExitFailure 1)

interpretIO :: Make a -> ExceptT ShellOutput IO a
interpretIO =
  foldFree interpret
  where
    interpret :: MakeF a -> ExceptT ShellOutput IO a
    interpret prog' =
      case prog' of
          Or inner next -> do
            foldFree interpret inner `catchError` \shellOutput -> foldFree interpret (next shellOutput)
          Shell cmd opts stdin next -> do
            let cmdOptions = case stdin of
                  Just str -> [Cmd.Stdin str]
                  _ -> []
            -- liftIO $ Prelude.print (cmd,cmdOptions, opts)
            liftIO (Cmd.command cmdOptions cmd opts)
              >>= \case
                  (Cmd.Exit ExitSuccess, Cmd.Stdout stdout, Cmd.Stderr stderr) ->
                      pure $ next (ShellOutput ExitSuccess stdout stderr)
                  (Cmd.Exit failure, Cmd.Stdout stdout, Cmd.Stderr stderr) -> 
                      throwError (ShellOutput failure stdout stderr)
          Print str a -> liftIO (Prelude.putStrLn str) >> pure a
          Exit ExitSuccess next -> pure $ next (fromExitCode ExitSuccess)
          Exit failure _ -> throwError (fromExitCode failure)

main :: IO ()
main = void $ runExceptT $ interpretIO $ do
  pgInstance <- runPostgres "1mypostgres" "mysecretpassword"
  case pgInstance of
    Just pg -> makePrint "got pg" >> stopPostgres (pgDockerName pg)
    Nothing -> makeExit (ExitFailure 1) 



