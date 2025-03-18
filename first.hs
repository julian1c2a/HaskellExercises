import System.Environment
import Data.Ratio
import Fact (fact, euler, euleraprox)

main :: IO ()
main = do
    let arg         = 20
    let arg_str     = show arg
    let result      = fact arg
    let result_str  = show result
    let finish      = "factorial (" ++ arg_str ++ ") = " ++ result_str
    putStrLn finish 
    let arg         = 17
    let arg_str     = show arg
    let result      = euler arg
    let result_str  = show result
    let finish      = "exponential_e (" ++ arg_str ++ ") = " ++ result_str
    putStrLn finish
    let arg         = 17 
    let arg_str     = show arg
    let result      = euleraprox arg
    let result_str  = show result
    let finish      = "exponential_e (" ++ arg_str ++ ") = " ++ result_str
    putStrLn finish
