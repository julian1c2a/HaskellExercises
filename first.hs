import System.Environment
import Data.Ratio
import Calcs

main :: IO ()
main = do
    let arg         = 20
    let arg_str     = show arg
    let result      = Calcs.Calcs arg
    putStrLn $ "Calcsorial (" ++ show arg ++ ") = " ++ show result

    let arg1 = 17
    let result1 = Calcs.euler arg1
    putStrLn $ "exponential_e (" ++ show arg1 ++ ") = " ++ show result1

    let arg2 = 17
    let result2 = Calcs.euleraprox arg2
    putStrLn $ "exponential_e (" ++ show arg2 ++ ") = " ++ show result2
