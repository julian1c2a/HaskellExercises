#include <iostream>
#include <iomanip>

#ifdef __GLASGOW_HASKELL__
#include "HsFFI.h"
#else
#include <windows.h>
#endif
#include "Calcs_c.h"

// Función helper para imprimir racionales
void printRational(const Rational_C& r) {
    if (r.is_negative) {
        std::cout << "-";
    }
    if (r.denominator == 1) {
        std::cout << r.numerator;
    } else {
        std::cout << r.numerator << "/" << r.denominator;
    }
}

int main(int argc, char *argv[]) {
    #ifdef __GLASGOW_HASKELL__
        // Inicializar el runtime de Haskell
        hs_init(&argc, &argv);
    #else
        std::cout << "Not compiling with GHC, skipping hs_init" << std::endl;
    #endif
    
    // 1. Probar fact_c
    std::cout << "== Función factorial ==" << std::endl;
    for (int i = 0; i <= 10; i++) {
        std::cout << "fact(" << i << ") = " << fact_c(i) << std::endl;
    }
    std::cout << std::endl;
    
    // 2. Probar euler_c
    std::cout << "== Aproximación del número e ==" << std::endl;
    for (int i = 0; i <= 10; i++) {
        std::cout << "euler(" << i << ") = " << std::fixed << std::setprecision(10) << euler_c(i) << std::endl;
    }
    std::cout << std::endl;
    
    // 3. Probar binom_c
    std::cout << "== Coeficientes binomiales ==" << std::endl;
    int n = 10;
    std::cout << "Fila " << n << " del triángulo de Pascal:" << std::endl;
    for (int k = 0; k <= n; k++) {
        std::cout << "C(" << n << "," << k << ") = " << binom_c(n, k) << std::endl;
    }
    std::cout << std::endl;
    
    // 4. Probar bern_c
    std::cout << "== Números de Bernoulli ==" << std::endl;
    Rational_C bernResult;
    for (int i = 0; i <= 10; i++) {
        bern_c(i, &bernResult);
        std::cout << "B_" << i << " = ";
        printRational(bernResult);
        std::cout << std::endl;
    }
    std::cout << std::endl;
    
    // 5. Probar fnJC_c
    std::cout << "== Números de Julián-Calderón ==" << std::endl;
    Rational_C jcResult;
    for (int n = 1; n <= 5; n++) {
        std::cout << "Fila n=" << n << ":" << std::endl;
        for (int m = 1; m <= n; m++) {
            fnJC_c(n, m, &jcResult);
            std::cout << "JC(" << n << "," << m << ") = ";
            printRational(jcResult);
            std::cout << std::endl;
        }
        std::cout << std::endl;
    }

    #ifdef __GLASGOW_HASKELL__
        // Cerrar el runtime de Haskell
        hs_exit();
    #endif

    return 0;
}