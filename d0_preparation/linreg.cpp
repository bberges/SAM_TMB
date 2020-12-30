#include <TMB.hpp >

template < class Type >
Type objective_function <Type >:: operator () ()
{
	DATA_VECTOR (x)
	DATA_VECTOR (y);

	PARAMETER ( alpha );
	PARAMETER ( beta );
	PARAMETER ( logSigma );
	vector <Type > pred = alpha + beta *x;
	Type ans =-sum( dnorm (y,pred ,exp ( logSigma ),true ));
	ADREPORT ( pred );

	return ans;
}