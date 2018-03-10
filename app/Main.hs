{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import           Data.Default.Class
import           Data.Monoid
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Options.Applicative                  hiding ((<|>))
import qualified Web.Scotty                           as Scotty

main :: IO ()
main =
  startServer =<< execParser (info (optionsParser <**> helper) modifiers)

startServer :: ServerConfig -> IO ()
startServer cfg@Cfg{..} = do
  putStrLn $ "Waiter: Serving '" ++ root ++ "' on port " ++ show port ++ "!"
  Scotty.scottyOpts (def { Scotty.verbose = 0, Scotty.settings = setPort port defaultSettings }) $
    forM_ (buildMiddleware cfg) Scotty.middleware

buildMiddleware :: ServerConfig -> [Middleware]
buildMiddleware cfg@Cfg{..}
  | loggingDisabled = [staticPolicy $ buildPolicy cfg]
  | otherwise       = [logStdout, staticPolicy $ buildPolicy cfg]

buildPolicy :: ServerConfig -> Policy
buildPolicy Cfg{..} = addBase root <> policy indexPolicy
  where indexPolicy path
          | path == root        = Just indexFile
          | path == root ++ "/" = Just indexFile
          | otherwise           = Just path
        indexFile = root ++ "/index.html"

data ServerConfig = Cfg {
    root            :: String
  , port            :: Int
  , loggingDisabled :: Bool
  }

optionsParser :: Parser ServerConfig
optionsParser = Cfg <$>
  strArgument (
       help "The path to a directory that will be served"
    <> value "."
    <> metavar "root"
    <> showDefaultWith (const "current directory")
  ) <*>
  option auto (
       help "The port on which to serve files"
    <> short 'p'
    <> long "port"
    <> value 3000
    <> showDefault
  ) <*>
  switch (
       help "Flag to disable request logging"
    <> long "no-log"
  )

modifiers :: InfoMod ServerConfig
modifiers =
     fullDesc
  <> header "Waiter - Fast Static File Serving"
  <> progDesc description

description =
  "Serves files from the specified directory."
