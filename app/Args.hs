{-
-- EPITECH PROJECT, 2023
-- Koaky
-- File description:
-- args
-}


module Args
    (
        Action(..),
        Args(..),
        RunOpt(..),
        parseArgs,
        printHelp
    ) where

data Action = ShowHelp | ShowVersion | Run
data RunOpt = RunStdin | RunFile

data Args = Args {
    action :: Action,
    runOpt :: RunOpt,
    filePath :: String
}

parseArgs' :: [String] -> Args -> Either Args String
parseArgs' [] args =
    Left args
parseArgs' ("--help":xs) args =
    parseArgs' xs (args {action = ShowHelp})
parseArgs' ("-h":xs) args =
    parseArgs' xs (args {action = ShowHelp})
parseArgs' ("--version":xs) args =
    parseArgs' xs (args {action = ShowVersion})
parseArgs' ("-v":xs) args =
    parseArgs' xs (args {action = ShowVersion})
parseArgs' ("-f":f:xs) args =
    parseArgs' xs (args {action = Run, runOpt = RunFile, filePath = f})
parseArgs' ["-f"] _ =
    Right "No file specified"
parseArgs' ("-":xs) args =
    parseArgs' xs (args {action = Run, runOpt = RunStdin})
parseArgs' (x:_) _ =
    Right ("Unknown option: " ++ x)

parseArgs :: [String] -> Either Args String
parseArgs args =
    parseArgs' args (Args {action = Run, runOpt = RunStdin, filePath = ""})

printHelp :: IO ()
printHelp = putStr help
    where
        line1 = "Usage: koaky [OPTION]\n\n"
        line2 = "Interpret Lisp\n"
        line2a = "With no options, koaky reads from standard input.\n\n"
        line3 = "Options:\n"
        line4 = "\t-h, --help\n\t\tDisplay this help and exit\n"
        line5 = "\t-v, --version\n\t\tOutput version information and exit\n"
        line6 = "\t-f FILE, --file FILE\n\t\tRead FILE and Interpret it\n"
        line7 = "\t-\n\t\tRead from standard input and Interpret it\n"
        help1 = line1 ++ line2 ++ line2a
        help2 = line3 ++ line4 ++ line5 ++ line6 ++ line7
        help = help1 ++ help2 
