#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(year)
  DATA_VECTOR(ssb)
  DATA_VECTOR(Robs)
  DATA_INTEGER(minAge)    
  DATA_INTEGER(mode)
    
  PARAMETER(logsdo);
  PARAMETER(logsdp);
  PARAMETER_VECTOR(logR);
  PARAMETER_VECTOR(rickerpar);
  PARAMETER_VECTOR(bhpar);
  Type sdo = exp(logsdo);
  Type sdp = exp(logsdp);

  Type jnll=0;

  Type pred;
  for(int i=1; i<logR.size(); ++i){
    switch(mode){
      case 0: // RW
        // your code to calculate prediction 
		pred = logR(i-1);//+dnorm(0,0,sdp,true);
      break;

      case 1: // Ricker
		pred = rickerpar(0)+log(ssb(i-minAge))-exp(rickerpar(1))*ssb(i-minAge);
      break;

      case 2: // B-H
	  pred = bhpar(0)+log(ssb(i-minAge))-log(Type(1)+exp(bhpar(1))*ssb(i-minAge));
      break;

      default:
	std::cout<<"Stock recruitment code not implemented yet."<<std::endl;
      break;
    }
	jnll += -dnorm(logR(i),pred,sdp,true);
    // your code to calculate process likelihood
  }
  for(int i=0; i<Robs.size(); ++i){
	jnll += -dnorm(log(Robs(i)),logR(i),sdo,true);
    // your code to calculate observation likelihood 
  }
  return jnll;
}
