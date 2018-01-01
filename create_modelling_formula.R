create_modelling_formula <- function(response,dependent_vars){
  return(as.formula(paste(response,"~",paste(dependent_vars,collapse="+"),sep="")))
}
