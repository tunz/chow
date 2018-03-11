module Main where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Semigroup      ((<>))
import           Data.Time
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Posix.Files

import           Chow.Checker
import           Chow.Common

data Args = Args
  { checker :: Maybe String
  , lang    :: Maybe String
  , isDir   :: Bool
  , path    :: String
  }

main :: IO ()
main =
  execParser opts >>= parseArgs >>= checkTime <$> runChecker >>=
  mapM_ (putStrLn . snd)
  where
    opts =
      info
        (args <**> helper)
        (fullDesc <> progDesc "Static Analysis Checker with Micro-Grammar")
    args =
      Args <$>
      optional (strOption (long "checker" <> short 'c' <> help "Checker")) <*>
      optional
        (strOption (long "lang" <> short 'l' <> help "Programming Language")) <*>
      switch
        (long "isdir" <> short 'd' <> help "The given file path is directory") <*>
      argument str (metavar "path" <> help "File or Directory Path")
    parseArgs (Args checker lang isDir path) =
      if langType == LangUnknown
        then failAndExit "Please specify a programming language"
        else putStrLn ("Language: " ++ show langType) >>
             putStrLn ("Checker: " ++ show ckType) >>
             fmap (Options ckType langType) getPaths
      where
        ckType = detectChecker checker
        detectChecker _ = TypeNone
        langType = detectLang $ lowerString lang
        detectLang "c"   = LangC
        detectLang "c++" = LangCpp
        detectLang "cpp" = LangCpp
        detectLang "cc"  = LangCpp
        detectLang _     = LangUnknown
        lowerString (Just str) = [toLower c | c <- str]
        lowerString Nothing    = lowerString (Just fileExt)
        fileExt = safeTail $ takeExtension path
        safeTail ""     = ""
        safeTail (x:xs) = xs
        getPaths
          | not isDir = return [path]
          | isDir = traverseDir path findExtFiles
        findExtFiles x = any (`endsWith` x) (extsFrom langType)
        extsFrom LangC   = [".c"]
        extsFrom LangCpp = [".c", ".cpp", ".cc"]
        endsWith endstr []  = False
        endsWith endstr str = endstr == str || endsWith endstr (tail str)
        traverseDir top include = do
          ds <- listDirectory top
          paths <-
            forM ds $ \d -> do
              let path = top </> d
              s <- getSymbolicLinkStatus path
              if isDirectory s
                then traverseDir path include
                else (if include path
                        then return [path]
                        else return [])
          return (concat paths)
    checkTime fn = do
      start <- getCurrentTime
      result <- fn
      stop <- getCurrentTime
      putStr "Running Time: "
      print $ diffUTCTime stop start
      return result
