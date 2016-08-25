#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;
#include "ragram.h"

// [[Rcpp::export(name = 'getStandardDeviationCpp')]]
double getStandardDeviation(NumericMatrix img, int u, int v, int n) {
    double sd = 0;
    double avg = getAverage(img, u, v, n);
    for (int i=-n-1; i<n; i++) {
        for (int j=-n-1; j<n; j++) {
            //if(!NumericVector::is_na(img(u+i, v+j)))
            sd += (img(u+i, v+j) - avg) * (img(u+i, v+j) - avg);
        }
    }
    return(sqrt(sd));
}