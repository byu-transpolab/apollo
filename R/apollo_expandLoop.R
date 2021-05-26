#' Expands loops in a function or expression
#' 
#' Expands loops replacing the index by its value. It also evaluates \code{paste} and \code{paste0}, and removes \code{get}.
#' 
#' For example, the expression
#' \code{for(j in 1:3) V[[paste0('alt',j)]] = b1*get(paste0('x',j)) + b2*X[,j]}
#' would be expanded into
#' \code{
#' V[[alt1]] = b1*x1 + b2*X[,1]
#' V[[alt2]] = b1*x2 + b2*X[,2]
#' V[[alt3]] = b1*x3 + b2*X[,3]
#' }
#' 
#' @param f A function or an expression
#' @param defs A named list of expressions to replace inside the loop. NULL by default.
#' @param env An environment with additional values to use in the loop expansion
#' @return A function or an expression (same type as input \code{f})
#' @export
apollo_expandLoop <- function(f, defs=NULL, env=NULL){
  #### Utilities ####
  
  is.val <- function(e) if(is.symbol(e) || is.numeric(e) || is.character(e) || is.logical(e) ) return(TRUE) else return(FALSE)
  
  replaceByDef <- function(e, defs, rightSide=FALSE){
    isFunction <- is.function(e)
    if(isFunction){f <- e; e <- body(e)}
    # Case 1: x
    test1 <- is.symbol(e) && (as.character(e) %in% names(defs))
    if(test1 && rightSide) e <- defs[[as.character(e)]]
    # Case 2: L$x or L[['x']] or L[["x"]]
    test2 <- !test1 && is.call(e) && length(e)==3 && is.symbol(e[[1]]) && (as.character(e[[1]]) %in% c('$','[['))
    test2 <- test2 && is.val(e[[3]]) && is.symbol(e[[2]])
    if(test2) tmp  <- paste0(as.character(e[[2]]), '$', as.character(e[[3]]))
    test2 <- test2 && (tmp %in% names(defs))
    if(test2 && rightSide) e <- defs[[tmp]]
    # Case 3: expression
    if( !test1 && !test2 && (is.call(e) || is.expression(e)) ){
      isAssign <- length(e)==3 && is.symbol(e[[1]]) && (as.character(e[[1]]) %in% c('<-', '='))
      for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- replaceByDef(e[[i]], defs, rightSide=(rightSide | (isAssign & i==3)))
    } 
    # Return
    if(isFunction){body(f) <- e; e <- f}
    return(e)
  }
  
  replaceIndex <- function(e, jNam, jVal){
    # Case 1: is j
    if(is.symbol(e) && e==jNam) return(jVal)
    # Case 2: is a value but not j
    if(is.val(e)) return(e)
    # Case 3: is an expression
    if(is.expression(e) || is.call(e)) for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- replaceIndex(e[[i]], jNam, jVal)
    return(e)
  }
  
  runPaste <- function(e){
    # Case 1: just a value
    if(is.val(e)) return(e)
    # Case 2: a call to paste or paste0
    test1 <- is.expression(e) || is.call(e)
    test1 <- test1 && length(e)>1 && is.symbol(e[[1]]) && (as.character(e[[1]]) %in% c('paste', 'paste0'))
    if(test1){
      if(length(all.vars(e))>0) stop('Unknown variables inside "paste" (can only use index).')
      e <- eval(e)
      return(e)
    }
    # Case 3: an expression
    test2 <- (is.call(e) || is.expression(e)) && !test1
    if(test2) for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- runPaste(e[[i]])
    return(e)
  }
  
  rmGet <- function(e){
    # Case 1: just a value
    if(is.val(e)) return(e)
    # Case 2: a call to get
    test1 <- is.expression(e) || is.call(e)
    test1 <- test1 && length(e)>1 && is.symbol(e[[1]]) && as.character(e[[1]])=='get'
    if(test1){
      if(length(all.vars(e))>0) stop('Unknown variables inside "get" (can only use index).')
      test3 <- length(e)==2 && is.character(e[[2]])
      if(!test3) stop('Could not build variable name fetched by "get".')
      e <- as.symbol(e[[2]])
      return(e)
    }
    # Case 3: an expression
    test2 <- (is.call(e) || is.expression(e)) && !test1
    if(test2) for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- rmGet(e[[i]])
    return(e)
  }
  
  evalIndex <- function(e, env=NULL){
    # Case 1: just a avalue
    if(is.val(e)) return(e)
    # Case 2: a call to '[' or '[['
    test <- (is.expression(e) || is.call(e)) && length(e)==3 && is.symbol(e[[1]]) && as.character(e[[1]]) %in% c('[', '[[')
    if(test){
      # Case 2.1: Index is a value
      if(is.val(e[[3]])) return(e)
      # Case 2.2: Index is an expression without variables
      if(length(all.vars(e[[3]]))==0){
        e[[3]] <- eval(e[[3]])
        return(e)
      }
      # Case 2.3: Index is an expression with variables
      if(length(all.vars(e[[3]]))>0){
        if(!is.null(env)){
          e[[3]] <- tryCatch(eval(e[[3]], envir=env), error=function(e) NULL)
        } else e[[3]] <- NULL
        if(is.null(e[[3]])) stop('expandLoop: Unknown variable inside index')
        return(e)
      }
    }
    # Case 3: an expression
    test2 <- (is.call(e) || is.expression(e)) && !test
    if(test2) for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- evalIndex(e[[i]], env=env)
    return(e)
  }
  
  expandLoop <- function(e, defs=NULL){
    # Initialise
    isF <- is.function(e)
    if(isF){f <- e; e <- body(e)}
    # Case 1: it is a symbol
    if(is.val(e)) return(e)
    # Case 2: it is a "for" call
    test1 <- is.call(e) || is.expression(e)
    test1 <- length(e)==4 && is.symbol(e[[1]]) && as.character(e[[1]])=='for'
    if(test1){
      if(!is.null(defs)) e <- replaceByDef(e, defs)
      jSym <- e[[2]] # index
      if(!is.null(defs)) e[[3]] <- replaceByDef(e[[3]], defs, rightSide=TRUE)
      if(length(all.vars(e[[3]]))>0) stop('Inside loops, the range of the index must not depend on any variable, but must be hardcoded (e.g. j in 1:100, not j in 1:J).')
      jValues <- eval(e[[3]]) # all values that j needs to take
      ee <- str2lang(paste0('{', paste0(jValues, collapse='; '), '}'))
      for(j in jValues){
        ee[[1+j]] <- replaceIndex(e[[4]], jSym, j)
        ee[[1+j]] <- runPaste(ee[[1+j]])
        ee[[1+j]] <- rmGet(ee[[1+j]])
        ee[[1+j]] <- evalIndex(ee[[1+j]], env)
      }
      if(!is.null(defs)) ee <- replaceByDef(ee, defs)
      return(ee)
    }
    # Case 3: It is a call but not a for
    test2 <- (is.call(e) || is.expression(e)) && !test1
    if(test2) for(i in 1:length(e)) if(!is.null(e[[i]])) e[[i]] <- expandLoop(e[[i]], defs)
    if(!isF) return(e) else {body(f) <- e; return(f)}
  }
  
  #### Return ####
  return(expandLoop(f, defs))
}



#rm(list=ls())
#f=function(x){
#  J = 100
#  V = list()
#  for(j in 1:J) V[[paste0("alt",j)]] = b1*get(paste0("x1_",j)) + b2*get(paste0("x2_",j))
#  # 1) Run paste
#  #V[["altj"]] = b1*get("x1_j") + b2*get("x2_j")
#  ## 2) Get rid of get
#  #V[["altj"]] = b1*x1_j + b2*x2_j
#  
#  #X1 = apollo_inputs$database[,paste0('tt', 1:100)]
#  #for(j in 1:J) V[[paste0("alt",j)]] = b1*apollo_inputs$database[,paste0('tt', j)] + b2*X2[,j]
#  ## 1)
#  #V[[paste0("alt",j)]] = b1*apollo_inputs$database[,"ttj"] + b2*X2[,j]
#  return(V)
#}
#defs <- apollo_varList(f, list(apollo_control=list(mixing=FALSE), apollo_randCoeff=NA))
#apollo_expandLoop(f, defs)
#

#e <- expression(for(j in 1:10) V[[paste0("alt",j)]] = b1*get(paste0("x1_",j)) + b2*get(paste0("x2_",j)))[[1]]
#replaceIndex(e, as.symbol('j'), 10)
#runPaste(replaceIndex(e, as.symbol('j'), 5))
#expandLoop(e)
#expandLoop(apollo_probabilities)