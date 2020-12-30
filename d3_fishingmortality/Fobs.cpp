#include <TMB.hpp>

template <class Type>
Type itrans(Type x){ // scaled ilogit
  return Type(2)/(Type(1) + exp(-Type(2) * x)) - Type(1);
}


template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(year)
  DATA_VECTOR(age)
  DATA_MATRIX(Fobs)
  DATA_INTEGER(cormode)
  int nrow=Fobs.rows();
  int ncol=Fobs.cols();

  PARAMETER_VECTOR(logsdF)
  PARAMETER_VECTOR(transPsi)
  PARAMETER(logsd)
  PARAMETER_MATRIX(logF)
  vector<Type> sdF = exp(logsdF);
  Type sd = exp(logsd);
  Type psi = 0;

  Type jnll=0;

  matrix<Type> Sigma(ncol,ncol);
  Sigma.setZero();

  switch(cormode){

    case 0:
      Sigma.diagonal() = sdF*sdF;
    break;

    case 1:
	// TODO
	Sigma.diagonal() = sdF*sdF;
	psi = itrans(transPsi(0));
	for(int i=0; i<ncol; ++i){
		for(int j=0; j<i; j++){
			Sigma(i,j) = psi*sdF(i)*sdF(j);
			Sigma(j,i) = psi*sdF(i)*sdF(j);
		}
	}
    break;

    case 2:
	//TODO
	Sigma.diagonal() = sdF*sdF;
	psi = itrans(transPsi(0));
	for(int i=0; i<ncol; ++i){
		for(int j=0; j<i; j++){
			Sigma(i,j) = pow(psi,abs(age(i)-age(j)))*sdF(i)*sdF(j);
			Sigma(j,i) = pow(psi,abs(age(i)-age(j)))*sdF(i)*sdF(j);
		}
	}
    break;

    default:
      std::cout<<"Error: This cormode not implemented yet."<<std::endl;
      exit(EXIT_FAILURE);
    break;
  }

  density::MVNORM_t<Type> nldens(Sigma);
  for(int y=1; y<nrow; ++y){
    jnll += nldens(logF.row(y)-logF.row(y-1));
  }

  for(int y=0; y<nrow; ++y){
    for(int a=0; a<ncol; ++a){
      jnll += -dnorm(log(Fobs(y,a)), logF(y,a), sd, true);
    }
  }
  return jnll;
}


/* For the AR(1) model it is possible to write the process part as:

    using namespace density;
    VECSCALE_t<AR1_t<N01<Type> > > nlAR=VECSCALE(AR1(itrans(transPsi(0))),sdF);
    for(int y=1; y<nrow; ++y){
      jnll += nlAR(logF.row(y)-logF.row(y-1));
    }

*/
