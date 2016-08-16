#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export(name = '.getAverage')]]
double getAverage(NumericMatrix img, int u, int v, int n) {
    double avg = 0;
    for (int i=-n-1; i<n; i++) {
        for (int j=-n-1; j<n; j++) {
            //if(!NumericVector::is_na(img(u+i, v+j)))
            avg += img(u+i, v+j);
        }
    }
    return(avg/((2*n+1)*(2*n+1)));
}

// [[Rcpp::export(name = '.getStandardDeviation')]]
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


//' Zero normalized cross correlation (ZNCC)
//' 
//' Calculates the zero normalized cross correlation (ZNCC) coefficient between two \code{\link{SAR-class}} objects.
//' This method also works for \code{\link{RasterLayer-class}} objects. The maximum ZNCC value determines disparity.
//'  
//' @export
// [[Rcpp::export]]
double znccCpp(NumericMatrix master, NumericMatrix slave, 
            int u1, int v1, int u2, int v2, int n) {
    
    double avg1 = getAverage(master, u1, v1, n);
    double avg2 = getAverage(slave, u2, v2, n);
    
    double sd1 = getStandardDeviation(master, u1, v1, n);
    // if(sd1==0) 
    //     return(0);
    double sd2 = getStandardDeviation(slave, u2, v2, n);
    // if(sd2==0)
    //     return(0);
    
    double cov = 0;
    
    for(int i=-n-1; i<n; i++) {
        for(int j=-n-1; j<n; j++) {
            //if(!NumericVector::is_na(master(u1+i, v1+j)) && !NumericVector::is_na(slave(u2+i, v2+j)))
            cov += (master(u1+i,v1+j) - avg1) * (slave(u2+i,v2+j) - avg2);
        }
    }
    return(cov/(sd1*sd2));
}

// [[Rcpp::export(name = '.znccPyramidal')]]
double znccPyramidal(NumericMatrix master, NumericMatrix slave, 
                     int u1, int v1, int u2, int v2, NumericVector n) {
    
    size_t len = n.size();
    
    double znccPyr;
    
    for(int k=0; k<len; k++) {
        
    double avg1 = getAverage(master, u1, v1, n(k));
    double avg2 = getAverage(slave, u2, v2, n(k));
    
    double sd1 = getStandardDeviation(master, u1, v1, n(k));
    // if(sd1==0) 
    //     return(0);
    double sd2 = getStandardDeviation(slave, u2, v2, n(k));
    // if(sd2==0)
    //     return(0);
    
    double cov = 0;
    
    for(int i=-n(k)-1; i<n(k); i++) {
        for(int j=-n(k)-1; j<n(k); j++) {
            //if(!NumericVector::is_na(master(u1+i, v1+j)) && !NumericVector::is_na(slave(u2+i, v2+j)))
            cov += (master(u1+i,v1+j) - avg1) * (slave(u2+i,v2+j) - avg2);
        }
    }
    if(k==0) {
        znccPyr = cov/(sd1*sd2);
    } else {
        znccPyr = znccPyr * cov/(sd1*sd2);
    }
    }
    return(znccPyr);
}

// [[Rcpp::export(name = '.getZnccsInSearchArea')]]
NumericMatrix getZnccsInSearchArea(NumericMatrix master, NumericMatrix slave,
                                   NumericVector index, int u, int v, int n,
                                   NumericVector searchAreaShift) {
    int l, k;
    size_t len = index.size();
    
    NumericMatrix dispTemp(len,len);
    
    for(int i=0; i<len; i++) {
        for(int j=0; j<len; j++) {
            k=index(i);
            l=index(j);
            dispTemp(i,j) = znccCpp(master,slave,u,v,
                     (u+k+searchAreaShift(0)),
                     (v+l+searchAreaShift(1)), n);
        }
    }
    return(dispTemp);
}

// [[Rcpp::export(name = '.getPyramidalZnccsInSearchArea')]]
NumericMatrix getPyramidalZnccsInSearchArea(NumericMatrix master, NumericMatrix slave,
                                            NumericVector index, int u, int v, NumericVector n,
                                            NumericVector searchAreaShift) {
    int l, k;
    size_t len = index.size();
    
    NumericMatrix dispTemp(len,len);
    
    for(int i=0; i<len; i++) {
        for(int j=0; j<len; j++) {
            k=index(i);
            l=index(j);
            dispTemp(i,j) = znccPyramidal(master,slave,u,v,
                     (u+k+searchAreaShift(0)),
                     (v+l+searchAreaShift(1)), n);
        }
    }
    return(dispTemp);
}