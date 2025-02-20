#' Lists variables names and definitions used in a model
#' 
#' Returns a list containing the names and definitions of variables in apollo_probabilities, apollo_randCoeff and apollo_lcPars
#' 
#' It looks for variables in apollo_beta, apollo_randCoeff, draws, and 
#' apollo_probabilities. It returns them in a list ordered by origin.
#' 
#' @param apollo_probabilities Likelihood function of the whole model.
#' @param apollo_inputs List of arguments and settings generated by \link{apollo_validateInputs}.
#' @param V Named list of functions.
#' @param cpp Scalar logical. If TRUE, expressions are modified to match C++ syntax (e.g. x^y -> pow(x,y)). FALSE by default.
#' 
#' @return A list of expressions containing all definitions in apollo_randCoeff and apollo_probabilities
#' @export
apollo_varList <- function(apollo_probabilities, apollo_inputs){
  
  #### Useful functions ####
  is.val <- function(e) if(is.symbol(e) || is.numeric(e) || is.character(e) || is.logical(e) ) return(TRUE) else return(FALSE)
  #containsAssignment <- function(e){
  #  if(is.val(e)) return(FALSE)
  #  test <- length(e)==3 && is.symbol(e[[1]]) && (as.character(e[[1]]) %in% c('=', '<-'))
  #  if(test) return(TRUE)
  #  tmp <- rep(FALSE, length(e))
  #  for(i in 1:length(e)) if(!is.null(e[[i]])) tmp[i] <- containsAssignment(e[[i]])
  #  return( any(tmp) )
  #}
  # If argument is a function definition, returns only the body of the function. e.g. function(x) x^2 --> x^2
  getBody <- function(e){
    isFunDef <- (is.expression(e) || is.call(e)) && length(e)>=1
    isFunDef <- isFunDef && is.symbol(e[[1]]) && as.character(e[[1]])=='function'
    if(isFunDef) return(e[[3]]) else return(e)
  }
  # Read names and definitions of variables inside apollo_probabilities
  extractDef <- function(e, includeListName=TRUE){
    # If e is a function, extract its body
    if(is.function(e)) e <- body(e)
    # If it's a value, return immediately
    if(is.val(e)) return(NULL)
    # Check if it is an assignment
    isAssignment <- length(e)==3 && is.symbol(e[[1]]) && (e[[1]]=="=" || e[[1]]=="<-")
    lSingleVar   <- isAssignment && length(e[[2]])==1 && is.symbol(e[[2]])
    lListElem    <- isAssignment && length(e[[2]])==3 && is.symbol(e[[2]][[1]]) && (as.character(e[[2]][[1]]) %in% c('[[', '$'))
    rSingleVar   <- isAssignment && length(e[[3]])==1 && is.symbol(e[[3]])
    rExpression  <- isAssignment && (is.expression(e[[3]]) || is.call(e[[3]])) && length(e[[3]])>=1
    rList        <- rExpression  && is.symbol(e[[3]][[1]]) && as.character(e[[3]][[1]])=='list'
    rFunCall     <- rExpression  && is.symbol(e[[3]][[1]]) && (as.character(e[[3]][[1]]) %in% as.vector(lsf.str('package:apollo')))
    #  as.character(e[[3]][[1]]) %in% paste0('apollo_', c('avgIntraDraws', 'panelProd', 'avgInterDraws', 
    #                                                     'prepareProb', 'combineModels'))
    ans <- list()
    # Variable or expression into a single variable
    if(lSingleVar && !rList && !rFunCall){
      ans[[as.character(e[[2]])]] <- getBody(e[[3]])
      return(ans)
    }
    # List into a single variable: expand into one entry per element L$elem
    if(lSingleVar && rList && !rFunCall && length(e[[3]])>1){
      if(includeListName) tmp <- paste0(as.character(e[[2]]), '$') else tmp <- ''
      for(i in 2:length(e[[3]])) ans[[ paste0(tmp, names(e[[3]])[i]) ]] <- getBody(e[[3]][[i]])
      return(ans)
    }
    # Variable or expression into list element
    if(lListElem && !rList && !rFunCall && is.val(e[[2]][[3]])){
      if(includeListName) tmp <- paste0(as.character(e[[2]][[2]]), '$') else tmp <- ''
      ans[[paste0(tmp, as.character(e[[2]][[3]]))]] <- getBody(e[[3]])
      return(ans)
    }
    # List into list element
    if(lListElem && rList && !rFunCall && is.val(e[[2]][[3]])){
      tmp <- paste0(as.character(e[[2]][[3]]), '$')
      if(includeListName) tmp <- paste0(as.character(e[[2]][[2]]), '$', tmp)
      if(!is.null(names(e[[3]]))) tmp2 <- names(e[[3]]) else tmp2 <- 0:(length(e[[3]])-1)
      for(i in 2:length(e[[3]])) ans[[ paste0(tmp, tmp2[i]) ]] <- getBody(e[[3]][[i]])
      return(ans)
    }
    # Check if it is a return with a list inside defined in place. e.g: return(list(a=1, b=2))
    isReturn <- !isAssignment && !is.symbol(e) && length(e)==2 && is.symbol(e[[1]]) && as.character(e[[1]])=='return'
    retList  <- isReturn && is.call(e[[2]]) && length(e[[2]])>1 && is.symbol(e[[2]][[1]]) && as.character(e[[2]][[1]])=='list'
    defInPlace <- retList && length(e[[2]])>1
    if(defInPlace){
      for(i in 2:length(e[[2]])) ans[[ names(e[[2]])[i] ]] <- getBody(e[[2]][[i]])
      return(ans)
    }
    # If it's an expression or call but not an assignment
    if(!isAssignment && !isReturn && (is.expression(e) || is.call(e))){
      for(i in 1:length(e)) if(!is.null(e[[i]])) ans <- c(ans, extractDef(e[[i]], includeListName))
      return(ans)
    }
    # In any other case
    return(NULL)
  }
  replaceByDef <- function(e, defs){
    # Case 1: x
    test1 <- is.symbol(e) && (as.character(e) %in% names(defs))
    if(test1) e <- defs[[as.character(e)]]
    # Case 2: L$x or L[['x']] or L[["x"]]
    test2 <- !test1 && is.call(e) && length(e)==3 && is.symbol(e[[1]]) && (as.character(e[[1]]) %in% c('$','[['))
    test2 <- test2 && is.val(e[[3]]) && is.symbol(e[[2]])
    if(test2) tmp  <- paste0(as.character(e[[2]]), '$', as.character(e[[3]]))
    test2 <- test2 && (tmp %in% names(defs))
    if(test2) e <- defs[[tmp]]
    # Case 3: expression
    if( !test1 && !test2 && (is.call(e) || is.expression(e)) ){
      for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- replaceByDef(e[[i]], defs)
    } 
    # Return
    return(e)
  }
  
  # Load definitions in apollo_randCoeff and apollo_probabilities, and replace recursive defs
  defs <- list()
  if(apollo_inputs$apollo_control$mixing && is.function(apollo_inputs$apollo_randCoeff)){
    defs <- c(defs, extractDef(apollo_inputs$apollo_randCoeff, includeListName=FALSE))
    defs <- c(defs, extractDef(apollo_inputs$apollo_randCoeff, includeListName=TRUE))
  }
  if(is.function(apollo_inputs$apollo_lcPars)){
    defs <- c(defs, extractDef(apollo_inputs$apollo_lcPars, includeListName=FALSE))
  }
  defs <- c(defs, extractDef(apollo_probabilities))
  if(length(defs)>1) for(i in 2:length(defs)) defs[[i]] <- replaceByDef(defs[[i]], defs[1:(i-1)])
  
  return(defs)
}