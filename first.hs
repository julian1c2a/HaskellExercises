import System.Environment
import Data.Ratio
import Calcs ( binom , fact , euler , euleraprox )

main :: IO ()
main = do
    let arg0         = 20::Integer
    let result0      = fact arg0
    putStrLn $ "Fact   (" ++ show arg0 ++ ") = " ++ show result0
    let arg1         = 17::Integer
    let result1      = euler arg1
    putStrLn $ "exp_e^( " ++ show arg1 ++ ") = " ++ show result1
    let arg2         = 17::Integer
    let result2      = euleraprox arg2
    putStrLn $ "exp_e^( " ++ show arg2 ++ ") = " ++ show result2
    let arg3         = 17::Integer
    let arg4         =  9::Integer
    let result3      = binom arg3 arg4
    let high_str     = "binom (" ++ show arg3 ++ ") ("
    let low_str      = show arg4 ++ ") = " ++ show result3
    putStrLn $ high_str ++ low_str