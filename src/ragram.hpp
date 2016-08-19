#include <Rcpp.h>
using namespace Rcpp;

#if !defined(RAGRAM_H)
#define RAGRAM_H 1

double getAverage(NumericMatrix img, int u, int v, int n);
double getStandardDeviation(NumericMatrix img, int u, int v, int n);
NumericVector getAvgAndSD(NumericMatrix img, int u, int v, int n);

double zncc(NumericMatrix master, NumericMatrix slave, 
            int u1, int v1, int u2, int v2, int n);

NumericMatrix znccMatrix(NumericMatrix master, NumericMatrix slave,
                         NumericVector index, int u, int v, int n,
                         NumericVector searchAreaShift);

NumericVector disparityMap(NumericMatrix master, NumericMatrix slave,
                           NumericVector index, int n,
                           int uStart, int uEnd, int vStart, int vEnd,
                           NumericVector searchAreaShift);

double znccMultiplied(NumericMatrix master, NumericMatrix slave, 
                      int u1, int v1, int u2, int v2, NumericVector n);

NumericMatrix znccMultipliedMatrix(NumericMatrix master, NumericMatrix slave,
                                   NumericVector index, int u, int v, NumericVector n,
                                   NumericVector searchAreaShift);

#endif