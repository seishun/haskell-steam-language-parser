import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath

import CodeGenerator
import Generator.CSharpGen
import Parser.LanguageParser
import Parser.TokenAnalyzer

main :: IO ()
main = do
  env <- getEnvironment
  args <- getArgs
  let projectPath = lookup "SteamRE" env <|> listToMaybe args
  
  exists <- maybe (return False) doesDirectoryExist projectPath
  unless exists (error "Unable to find SteamRE project path, please specify the `SteamRE` environment variable")
  parseFile (fromJust projectPath)
            "Resources/SteamLanguage"
            "steammsg.steamd"
            "SteamKit2"
            "SteamKit2/SteamKit2/Base/Generated"
            "SteamLanguage"
            True
            cSharpGen
            "cs"

parseFile :: String -> String -> String -> String -> String -> String -> Bool -> CodeGen -> String -> IO ()
parseFile projectPath path file nspace outputPath outFile supportsGC codeGen fileNameSuffix = do
  let languagePath = projectPath </> path
  
  setCurrentDirectory languagePath
  tokenList <- tokenizeString <$> readFile (languagePath </> file)
  
  root <- analyze tokenList
  
  let rootEnumNode = Node [n | n@(EnumNode {}) <- childNodes root]
  let rootMessageNode = Node [n | n@(ClassNode {}) <- childNodes root]
  
  let enumWriter = execWriter $ emitCode rootEnumNode codeGen nspace supportsGC False
  let messageWriter = execWriter $ emitCode rootMessageNode codeGen (nspace ++ ".Internal") supportsGC True
  
  let outputEnumFile = outputPath </> (outFile ++ "." ++ fileNameSuffix)
  let outputMessageFile = outputPath </> (outFile ++ "Internal." ++ fileNameSuffix)
  
  writeFile (projectPath </> outputEnumFile) (unlines enumWriter)
  writeFile (projectPath </> outputMessageFile) (unlines messageWriter)
