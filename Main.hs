module Main where

import Render (render)
import Options.Applicative

data Opts = Opts
    { texs :: FilePath
    , envs :: FilePath
    , bumps :: FilePath
    , seeligerFlag :: Bool}

optParser :: Parser Opts
optParser = Opts 
            <$> strOption ( long "texture" <> short 't' <> metavar "TEXTURE")
            <*> strOption ( long "environment" <> short 'e' <> metavar "ENVIRONMENT")
            <*> strOption ( long "bumps"<> short 'b' <> metavar "BUMP")
            <*> switch ( long "seeliger")

main = execParser opts >>= runRender
    where
      opts = info (helper <*> optParser)
             ( fullDesc
               <> progDesc ""
               <> header "")
      runRender (Opts texs envs bumps lightFlag ) = render texs envs bumps lightFlag

