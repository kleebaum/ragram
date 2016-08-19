#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//' Window mean written in C++
//' 
//' Calculates window mean for a given \code{\link{SAR-class}} object.
//' The window is a square.
//' 
//' @param object Matrix.
//' @param u Integer. Center of window (column, i.e. y coordinate).
//' @param v Integer. Center of window (row, i.e. x coordinate).
//' @param n Integer. (Window size-1)/2 in x and y direction.
//' @export
// [[Rcpp::export(name = 'getAverageCpp')]]
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

// [[Rcpp::export(name = 'getAvgAndSDCpp')]]
NumericVector getAvgAndSD(NumericMatrix img, int u, int v, int n) {
    NumericVector avgAndSD(2);
    avgAndSD(0) = getAverage(img, u, v, n);
    double sd = 0;
    for (int i=-n-1; i<n; i++) {
        for (int j=-n-1; j<n; j++) {
            //if(!NumericVector::is_na(img(u+i, v+j)))
            sd += (img(u+i, v+j) - avgAndSD(0)) * (img(u+i, v+j) - avgAndSD(0));
        }
    }
    avgAndSD(1) = sqrt(sd);
    return(avgAndSD);
}

//' Zero normalized cross correlation (ZNCC) written in C++
//' 
//' Calculates the zero normalized cross correlation (ZNCC) coefficient between two \code{\link{SAR-class}} objects.
//' This method also works for \code{\link{RasterLayer-class}} objects. The maximum ZNCC value determines disparity.
//'  
//' @param master Matrix.
//' @param slave Matrix.
//' @param u1 Integer. Column, i.e. y coordinate of master pixel.
//' @param v1 Integer. Row, i.e. x coordinate of master pixel.
//' @param u2 Integer. Column, i.e. y coordinate of slave pixel.
//' @param v2 Integer. Row, i.e. x coordinate of slave pixel.
//' @param n Integer. (Window size-1)/2 in x direction.
//' @param m Integer. (Window size-1)/2 in y direction.
//' @export
// [[Rcpp::export(name = 'znccCpp')]]
double zncc(NumericMatrix master, NumericMatrix slave, 
            int u1, int v1, int u2, int v2, int n) {
    NumericVector avgAndSD1 = getAvgAndSD(master, u1, v1, n);
    NumericVector avgAndSD2 = getAvgAndSD(slave, u2, v2, n);
    
    double cov = 0;
    
    for(int i=-n-1; i<n; i++) {
        for(int j=-n-1; j<n; j++) {
            //if(!NumericVector::is_na(master(u1+i, v1+j)) && !NumericVector::is_na(slave(u2+i, v2+j)))
            cov += (master(u1+i,v1+j) - avgAndSD1(0)) * (slave(u2+i,v2+j) - avgAndSD2(0));
        }
    }
    return(cov/(avgAndSD1(1)*avgAndSD2(1)));
}

// [[Rcpp::export(name = 'znccMatrixCpp')]]
NumericMatrix znccMatrix(NumericMatrix master, NumericMatrix slave,
                         NumericVector index, int u, int v, int n,
                         NumericVector searchAreaShift) {
    size_t len = index.size();
    
    NumericMatrix znccMatrix(len,len);
    
    for(int i=0; i<len; i++) {
        for(int j=0; j<len; j++) {
            znccMatrix(i,j) = zncc(master,slave,u,v,
                       (u+index(i)+searchAreaShift(1)),
                       (v+index(j)+searchAreaShift(0)), n);
        }
    }
    return(znccMatrix);
}

// [[Rcpp::export(name = 'disparityMapCpp')]]
NumericVector disparityMap(NumericMatrix master, NumericMatrix slave,
                           NumericVector index, int n,
                           int uStart, int uEnd, int vStart, int vEnd,
                           NumericVector searchAreaShift) {
    
    NumericVector dim(2);
    dim(0) = uEnd-uStart+1;
    dim(1) = vEnd-vStart+1;
    
    NumericVector dispX(dim(0)*dim(1));
    dispX.attr("dim") = dim;
    NumericVector dispY(dim(0)*dim(1));
    dispY.attr("dim") = dim;
    
    int l, k;
    size_t len = index.size();
    
    NumericMatrix znccMatrix(len,len);
    
    for(int u=uStart; u<uEnd; u++) {
        for(int v=vStart; v<vEnd; v++) {
            for(int i=0; i<len; i++) {
                for(int j=0; j<len; j++) {
                    k=index(i);
                    l=index(j);
                    znccMatrix(i,j) = zncc(master,slave,u,v,
                               (u+k+searchAreaShift(1)),
                               (v+l+searchAreaShift(0)), n);
                }
            }
            
            int whichMin = which_min(znccMatrix);
            dispY(u-uStart,v-vStart) = index(whichMin % znccMatrix.nrow());
            int dispXTemp = 0;
            while(whichMin >= znccMatrix.ncol()) {
                dispXTemp += 1;
                whichMin = whichMin - znccMatrix.nrow();
            }
            dispX(u-uStart,v-vStart) = index(dispXTemp);
        }
    }
    
    NumericVector dimDisp(3);
    dimDisp(0) = dim(0);
    dimDisp(1) = dim(1);
    dimDisp(2) = 2;
    
    NumericVector disp(dim(0)*dim(1)*2);
    disp.attr("dim") = dimDisp;
    
    int indexDispY = dim(0)*dim(1);
    
    for(int i=0; i<indexDispY; i++)
        disp(i) = dispX(i);
    for(int i=0; i<indexDispY; i++)
        disp(i+indexDispY) = dispY(i);
    
    return(disp);
}

// [[Rcpp::export(name = 'znccMultipliedCpp')]]
double znccMultiplied(NumericMatrix master, NumericMatrix slave, 
                      int u1, int v1, int u2, int v2, NumericVector n) {
    size_t len = n.size();
    double zncc;
    
    for(int k=0; k<len; k++) {
        NumericVector avgAndSD1 = getAvgAndSD(master, u1, v1, n(k));
        NumericVector avgAndSD2 = getAvgAndSD(slave, u2, v2, n(k));
        
        double cov = 0;
        
        for(int i=-n(k)-1; i<n(k); i++) {
            for(int j=-n(k)-1; j<n(k); j++) {
                //if(!NumericVector::is_na(master(u1+i, v1+j)) && !NumericVector::is_na(slave(u2+i, v2+j)))
                cov += (master(u1+i,v1+j) - avgAndSD1(0)) * (slave(u2+i,v2+j) - avgAndSD2(0));
            }
        }
        if(k==0) {
            zncc = cov/(avgAndSD1(1)*avgAndSD2(1));
        } else {
            zncc = zncc * cov/(avgAndSD1(1)*avgAndSD2(1));
        }
    }
    return(zncc);
}

// [[Rcpp::export(name = 'znccMultipliedMatrixCpp')]]
NumericMatrix znccMultipliedMatrix(NumericMatrix master, NumericMatrix slave,
                                   NumericVector index, int u, int v, NumericVector n,
                                   NumericVector searchAreaShift) {
    size_t len = index.size();
    
    NumericMatrix dispTemp(len,len);
    
    for(int i=0; i<len; i++) {
        for(int j=0; j<len; j++) {
            dispTemp(i,j) = znccMultiplied(master,slave,u,v,
                     (u+index(i)+searchAreaShift(1)),
                     (v+index(j)+searchAreaShift(0)), n);
        }
    }
    return(dispTemp);
}