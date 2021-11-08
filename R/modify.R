#' Modify an Object In Place
#'
#' Update the value pointed to by a \code{\link{ref}} object. If the new value is a function,
#' the old values will be applied to the function and overwritten.
#'
#' @param x object of class \code{"ref"}
#' @param value new value or function applied to the object at the referenced location
#' @param ... additional arguments passed to the function
#'
#' @return object of class \code{"ref"}
#'
#' @export
#'
#' @examples
#' x <- 1:10
#' ref_to_x <- ref(x)
#'
#' # Apply the square root function
#' modify_by(ref_to_x, sqrt)
#' print(x)
#'
#' # Overwrite the original values
#' modify_by(ref_to_x, "hello world!")
#' print(x)
#'
modify_by <- function(x, value, ...) UseMethod("modify_by")
#' @export
#' @method modify_by ref
modify_by.ref <- function(x, value, ...){
  expr <- substitute(x <- z)
  expr[[2]] <- as.symbol(getSym(x))
  if (is.function(value)){
    dx <- deref(x)
    res <- do.call(value, append(list(dx), list(...)))
    expr[[3]] <- res
  } else {
    expr[[3]] <- value
  }
  eval(expr, envir=getEnv(x))
}
#' @export
#' @method modify_by sref
modify_by.sref <- function(x, value, ...){
  stop("sref objects cannot be modified.")
}
#' @export
#' @method modify_by slice
modify_by.slice <- function(x, value, ...){
  expr <- substitute(x[y] <- z)
  e <- getEnv(x)
  expr[[2]][[2]] <- as.symbol(getSym(x))
  expr[[2]][[3]] <- getIndex(x)
  if (is.function(value)){
    dx <- deref(x)
    expr[[3]] <- do.call(value, append(list(dx), list(...)))
  } else {
    expr[[3]] <- value
  }
  eval(expr, envir=e)
}
#' @export
#' @method modify_by default
modify_by.default <- function(x, value, ...){
  sub_x <- substitute(x, sys.frame(-1))
  e <- find_obj_(sub_x, sys.nframe()-2)
  expr <- substitute(x <- z)
  expr[[2]] <- sub_x
  if (is.function(value)){
    expr[[3]] <- as.call(append(list(value, sub_x), list(...)))
  } else { expr[[3]] <- value }
  eval(expr, envir=e)
}
