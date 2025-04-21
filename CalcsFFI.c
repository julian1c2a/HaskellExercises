#ifndef CALCS_C_H
#define CALCS_C_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Desde fact_c :: CLLong -> CLLong
long long  fact_c(long long  n);

// Desde euler_c :: CInt -> CDouble
double euler_c(int n);

// Desde binom_c :: CInt -> CInt -> CULLong
unsigned long long binom_c(int n, int k);

typedef struct {
    unsigned long long numerator;
    unsigned long long denominator;
    uint8_t is_negative;
} Rational_C;

// desde bern_c :: CInt -> Ptr RationalC -> IO ()
void bern_c(int n, Rational_C *rational_c);

// desde fnJC_c :: CInt -> CInt -> Ptr RationalC -> IO ()
void fnJC_c(int n, int k, Rational_C *rational_c);

#ifdef __cplusplus
}
#endif

#endif   /* CALCS_C_H */
