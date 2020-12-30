#include <TMB.hpp>
 
template<class Type>
Type objective_function<Type>::operator() ()
{
	DATA_VECTOR(SSB_pred)
	DATA_VECTOR(SSB)
	DATA_VECTOR(logR);

	PARAMETER(logA); 
	PARAMETER(logB);
	PARAMETER(logSigma);
	vector<Type> pred_model=logA+log(SSB)-log(Type(1)+exp(logB)*SSB);
	Type nl=-sum(dnorm(logR,pred_model,exp(logSigma),true));
	vector<Type> pred=logA+log(SSB_pred)-log(Type(1)+exp(logB)*SSB_pred);
	ADREPORT(pred)
	return nl;
}
