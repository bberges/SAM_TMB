#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_MATRIX(N)
  DATA_MATRIX(F)
  DATA_MATRIX(M)
  DATA_IMATRIX(aux)
  DATA_INTEGER(minYear)
  DATA_INTEGER(minAge)
  DATA_VECTOR(Cobs)
    
  int nobs=Cobs.size();
  PARAMETER_VECTOR(logsd)
  vector<Type> sd=exp(logsd);
    
  Type nll=0;
  
  vector<Type> logPred(nobs);
  Type Z;
  int y, a;
  for(int i=0; i<nobs; ++i){
    y=aux(i,0)-minYear;
    a=aux(i,2)-minAge;
    Z=F(y,a)+M(y,a);
	logPred(i)=log(F(y,a))-log(Z)+log(Type(1)-exp(-F(y,a)-M(y,a)))+log(N(y,a));
	nll += -dnorm(log(Cobs(i)), logPred(i), sd(a), true);
//    logPred(i) = ...;
//    nll += ...;
  }
  REPORT(logPred)
  return nll;
}
