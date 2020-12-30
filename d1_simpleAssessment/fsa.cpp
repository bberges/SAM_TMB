#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_IVECTOR(year)
  DATA_IVECTOR(fleet)
  DATA_IVECTOR(age)
  DATA_VECTOR(obs)
  DATA_ARRAY(stockMeanWeight)
  DATA_ARRAY(M)              
  DATA_ARRAY(propMature)     
  DATA_SCALAR(surveyTime)     

  PARAMETER_VECTOR(logN1Y);
  PARAMETER_VECTOR(logN1A);
  PARAMETER_VECTOR(logFY);
  PARAMETER_VECTOR(logFA);
  PARAMETER(logVarLogCatch);
  PARAMETER(logVarLogCatch1Y);
  PARAMETER_VECTOR(logQ1);
  PARAMETER_VECTOR(logQ2);
  PARAMETER(logVarLogSurvey);  

  int minAge=age.minCoeff();
  int maxAge=age.maxCoeff();
  int na=maxAge-minAge+1;
  int minYear=year.minCoeff();
  int maxYear=year.maxCoeff();
  int ny=maxYear-minYear+1;

  Type ans=0;

  // setup F
  matrix<Type> F(na,ny);
  for(int a=0; a<na; ++a){
    for(int y=0; y<ny; ++y){
      F(a,y)=exp(logFY(y))*exp(logFA(a));
    }
  }
  // setup logN
  matrix<Type> logN(na,ny);
  for(int a=0; a<na; ++a){
    logN(a,0)=logN1Y(a);
  } 
  for(int y=1; y<ny; ++y){
    logN(0,y)=logN1A(y-1);
    for(int a=1; a<na; ++a){
		if(a == na-1){
			logN(a,y)=log(exp(logN(a-1,y-1)-F(a-1,y-1)-M(a-1,y-1))+exp(logN(a,y-1)-F(a,y-1)-M(a,y-1)));
		}else{
			logN(a,y)=logN(a-1,y-1)-F(a-1,y-1)-M(a-1,y-1);
		}
    }
  } 

  // Match to observations
  vector<Type> logObs=log(obs);
  Type pred, sd;
  int a, y;
  for(int i=0; i<logObs.size(); ++i){
    a = age(i)-minAge;
    y = year(i)-minYear;

    if(fleet(i)==1){
      pred = log(F(a,y))-log(F(a,y)+M(a,y))+log(Type(1.0)-exp(-F(a,y)-M(a,y)))+logN(a,y);
	  if(age(i) == minAge){
		  sd = exp(Type(0.5)*logVarLogCatch1Y);
	  }else{
		  sd = exp(Type(0.5)*logVarLogCatch);
	  }
    }else{
	  if(year(i) < 2000){
		  pred = logQ1(a)-(F(a,y)+M(a,y))*surveyTime+logN(a,y);
	  }else{
		  pred = logQ2(a)-(F(a,y)+M(a,y))*surveyTime+logN(a,y);
	  }
      sd = exp(Type(0.5)*logVarLogSurvey);
    }    
    ans += -dnorm(logObs(i),pred,sd,true);
  }

  vector<Type> ssb(ny);
  ssb.setZero();
  for(int y=0; y<ny; ++y){
    for(int a=0; a<na; ++a){
      ssb(y)+=exp(logN(a,y))*stockMeanWeight(a,y)*propMature(a,y);
    }
  }

  ADREPORT(ssb);
  ADREPORT(F);
  return ans;
}
