{-
-- EPITECH PROJECT, 2023
-- Leviator compiler
-- File description:
-- Args
-}


module Args
    (
        Action(..),
        Args(..),
        parseArgs,
        printHelp
    ) where

import System.Directory (getCurrentDirectory)

data Action = ShowHelp | ShowVersion | Run

data Args = Args {
    action :: Action,
    folderPath :: String,
    outFile :: String
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
parseArgs' ("-o":x:xs) args =
    parseArgs' xs (args {outFile = x})
parseArgs' ["-o"] _ =
    Right "Missing argument for -o"
parseArgs' (('-':xs):_) _ =
    Right ("Unknown option: " ++ xs)
parseArgs' (x:xs) args =
    parseArgs' xs (args {action = Run, folderPath = x})

parseArgs :: [String] -> IO (Either Args String)
parseArgs args =
    getCurrentDirectory >>= \path ->
    return (parseArgs' args (Args {
        action = Run, folderPath = path, outFile = "out.wasm"
    }))

hLine1 :: String
hLine1 = "Usage: lvtc [OPTION] [FOLDER]\n"
hLine2 :: String
hLine2 = "\n"
hLine3 :: String
hLine3 = "Compile Leviator source code to WebAssembly\n"
hLine4 :: String
hLine4 = ""
hLine5 :: String
hLine5 = "Options:\n"
hLine6 :: String
hLine6 = "\t-h, --help\n\t\tDisplay this help and exit\n"
hLine7 :: String
hLine7 = "\t-v, --version\n\t\tOutput version information and exit\n"
hLine8 :: String
hLine8 = "\t-o FILE\n\t\tWrite WebAssembly to FILE\n"
hLine9 :: String
hLine9 = part1 ++ part2
    where
        part1 = "\tFOLDER\n\t\tTake all Leviator"
        part2 = " source code recursively from FOLDER\n"

printHelp :: IO ()
printHelp =
    putStr hLine1 >> putStr hLine2 >> putStr hLine3 >> putStr hLine4
    >> putStr hLine5 >> putStr hLine6 >> putStr hLine7 >> putStr hLine8
    >> putStr hLine9
