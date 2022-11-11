
module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (takeBaseName, replaceExtension, takeDirectory)
import System.Process

import AbsInstant   (Program)
import LexInstant   (Token)
import ParInstant   (pProgram, myLexer)
import LLVMCompiler (compile)

type Err        = Either String
type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()
-- runFile parseFun filePath = putStrLn filePath >> readFile filePath >>= run parseFun
runFile parseFun filePath = do
    instantSourceCode <- readFile filePath
    let llvmSourceCodeFilePath = replaceExtension filePath "ll"
    let llvmByteCodeFilePath = replaceExtension filePath "bc"
    case run parseFun instantSourceCode of
        Left error -> hPutStrLn stderr error
        Right code -> do
            writeFile llvmSourceCodeFilePath code
            (exitcode, out, err) <- readProcessWithExitCode ("llvm-as") ["-o", llvmByteCodeFilePath, llvmSourceCodeFilePath] ""
            case exitcode of
                ExitSuccess ->
                    exitSuccess
                ExitFailure i -> do
                    hPutStrLn stderr $ "An error occurred (exit code: " ++ show i ++ ")"
                    hPutStrLn stderr out
                    hPutStrLn stderr err
                    exitFailure

run :: ParseFun Program -> String -> Either String String
run parseFun s = case parseFun ts of
    Left error -> Left error
    Right prog -> compile prog
  where
  ts = myLexer s

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    -- []         -> getContents >>= run pProgram
    fs         -> mapM_ (runFile pProgram) fs
