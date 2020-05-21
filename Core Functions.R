library(Rcpp)
library(inline)
cppFunction('List rcpp_lapply(List input, Function f) {

  R_xlen_t n = input.length();
  
  List out(n);
  
  for(R_xlen_t i = 0; i < n; ++i) {
    out[i] = f(input[i]);
  }
  return out;
}'
)

#########  CORRELATION FUNCTION #########
#1. Instead of base R version, uses formula to output correlation metric for each x and y
#2 . Defines numeric vectors ( I assume these are data types that enable "communication" with C++ and R")
#3. First Line defines a variable of the size of the second colummn, 
#4. Uses a for loop to iterate and correlate for each binary pair(x,y)
library(Rcpp)
{
  cppFunction('NumericVector corr(NumericVector X , NumericVector Y,NumericVector xavg, NumericVector yavg)
  {
    int n = Y.size();
    NumericVector Corr(n);
    
    for(int i = 0; i < n; ++i)
    {
 Corr[i] = sum(X[i]-xavg) * sum(Y[i]- yavg) / sqrt(sum(pow(X[i]-xavg,2))) * sqrt(sum(pow(Y[i]-yavg,2)));
    }
return Corr;
  }'
  )
}

## Alt if_else function, works better sometimes but might not be using at the moment(sourced from internet r blog)
decode <- function(x, search, replace, default = NULL) {
  
  # build a nested ifelse function by recursion
  decode.fun <- function(search, replace, default = NULL)
    if (length(search) == 0L) {
      function(x) if (is.null(default)) x else rep(default, length(x))
    } else {
      function(x) ifelse(x == search[1L], replace[1L],
                         decode.fun(tail(search,  -1L),
                                    tail(replace, -1L),
                                    default)(x))
    }
  
  return(decode.fun(search, replace, default)(x))
}
cpp_lapply <- cfunction(signature(x = "list", g = "function" ), '
		Function fun(g) ;
		List input(x) ;
		List output( input.size() ) ;
		std::transform( input.begin(), input.end(), output.begin(), fun ) ;
		output.names() = input.names() ;
		return output ;
	
	', Rcpp = TRUE, includes = "using namespace Rcpp;"  )


onehotencoder <- function(df_orig) {
  df<-cbind(df_orig)
  df_clmtyp<-data.frame(clmtyp=sapply(df,class))
  df_col_typ<-data.frame(clmnm=colnames(df),clmtyp=df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    if (df_col_typ[rownm,"clmtyp"]=="factor") {
      clmn_obj<-df[toString(df_col_typ[rownm,"clmnm"])] 
      dummy_matx<-data.frame(model.matrix( ~.-1, data = clmn_obj))
      dummy_matx<-dummy_matx[,c(1,3:ncol(dummy_matx))]
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
      df<-cbind(df,dummy_matx)
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
    }  }
  return(df)
}
