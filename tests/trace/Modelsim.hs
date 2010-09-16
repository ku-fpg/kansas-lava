module Modelsim where

import Language.KansasLava
import Language.KansasLava.Testing

import System.Cmd
import System.Directory
import System.FilePath.Posix
import System.Posix.Directory

-- this is the callback we will use when running deep thunks.
-- the user must supply this, and it can be customized to do
-- whatever they want with the testbench we generate
modelsim :: FilePath -> IO ()
modelsim path = do
    let name = last $ splitPath path

    cwd <- getWorkingDirectory
    changeWorkingDirectory path

    writeFile "run" $ unlines
        ["#!/bin/bash"
        ,"LM_LICENSE_FILE=1800@carl.ittc.ku.edu:1717@carl.ittc.ku.edu"
        ,"export LM_LICENSE_FILE"
        ,"echo \"Simulating " ++ name ++ "...\""
        ,"/tools/modelsim/linux/6.3c/modeltech/bin/vsim -c -do " ++ name ++ ".do"
        ,"echo \"10 lines from the info file...\""
        ,"tail " ++ name ++ ".info"
        ,"echo \"The same 10 lines from the shallow trace...\""
        ,"tail " ++ name ++ ".shallow"
        ,"echo \"Ditto for the deep trace...\""
        ,"tail " ++ name ++ ".deep"
        ,""
        ,"THEDIFF=`diff " ++ name ++ ".shallow " ++ name ++ ".deep`"
        ,""
        ,"if [[ -z \"$THEDIFF\" ]]; then"
        ,"    echo \"Shallow/Deep Traces Are The Same\""
        ,"    exit 0"
        ,"else"
        ,"    echo \"Warning: Differences Below:\""
        ,"    echo \"$THEDIFF\""
        ,"    exit 1"
        ,"fi"
        ]


    system $ "chmod +x run"
    system $ "./run"

    changeWorkingDirectory cwd
