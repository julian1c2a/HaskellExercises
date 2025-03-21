import System.Environment
import Data.Ratio
import Fact

main :: IO ()
main = do
    let arg         = 20
    let arg_str     = show arg
    let result      = Fact.fact arg
    putStrLn $ "factorial (" ++ show arg ++ ") = " ++ show result

    let arg1 = 17
    let result1 = Fact.euler arg1
    putStrLn $ "exponential_e (" ++ show arg1 ++ ") = " ++ show result1

    let arg2 = 17
    let result2 = Fact.euleraprox arg2
    putStrLn $ "exponential_e (" ++ show arg2 ++ ") = " ++ show result2
