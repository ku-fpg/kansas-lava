import Report
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do pname <- getProgName
                putStrLn "Need path to simulation directory."
                putStrLn $ "USAGE: " ++ pname ++ " path"
                putStrLn $ "Example: " ++ pname ++ " sims"
        else generateReport $ args !! 0

