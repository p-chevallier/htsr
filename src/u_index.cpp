//---------------------------------
//' @name u_index
//' @title Compute an index of community
//' @author P. Chevallier - April 2023
//' @param nz length of the concatenated time-series
//' @param yd initial vector of datetimes (in sec)
//' @details the function compute an index, which the number of apparition of the same datetime
//' in a time-series
//' @return vector of indexes

#include <chrono>
#include <thread>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector u_index(int nz, IntegerVector zd) {
  std::cout.precision(4);
  IntegerVector ze = zd;
  IntegerVector zi(nz);

  for(size_t i=0; i<nz; ++i) {

		for (int j = 0; j < nz; ++j) {
      if (ze[j] == zd[i]) zi[i] += 1;
    }

    if(i == nz - 1) {
      std::cout << "100.0%" << std::endl;
    } else if(i % 33 == 0) {
      std::cout << static_cast<double>(i+1) / static_cast<double>(nz) * 100.0 << "%";
      std::cout << "\r";
    }

  }
  return zi;
}
