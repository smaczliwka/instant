
module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (takeBaseName, replaceExtension, takeDirectory)
import System.Process

import AbsInstant   (Program)
import LexInstant   (Token)
import ParInstant   (pProgram, myLexer)
import JVMCompiler  (compile)

type Err        = Either String
type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()

runFile parseFun filePath = do
    instantSourceCode <- readFile filePath
    let className = takeBaseName filePath
    let jasminSourceCodeFilePath = replaceExtension filePath "j"
    let outputDirPath = takeDirectory filePath
    case run className parseFun instantSourceCode of
        Left error -> do
          hPutStrLn stderr error
          exitFailure
        Right code -> do
            writeFile jasminSourceCodeFilePath code
            (exitcode, out, err) <- readProcessWithExitCode ("java") ["-jar", "./lib/jasmin.jar", "-d", outputDirPath, jasminSourceCodeFilePath] ""
            case exitcode of
                ExitSuccess ->
                    exitSuccess
                ExitFailure i -> do
                    hPutStrLn stderr $ "An error occurred (exit code: " ++ show i ++ ")"
                    hPutStrLn stderr out
                    hPutStrLn stderr err
                    exitFailure

run :: String -> ParseFun Program -> String -> Either String String
run className parseFun s =
  case parseFun ts of
    Left error -> Left error
    Right prog -> compile className prog
  where
  ts = myLexer s

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (files)         Compile files."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    fs         -> mapM_ (runFile pProgram) fs

