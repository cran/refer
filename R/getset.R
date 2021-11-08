#' Extract or Replace Parts of a Referenced Object
#'
#' Operators acting on a \code{\link{ref}} object that extract part of the underlying object
#' at the supplied indices, or replaces parts. These operators modify or extract from the
#' object that is referenced, not the reference! Use \code{\link{sref}} is this behavior
#' is undesirable.
#'
#' @param x object of class \code{"ref"}
#' @param name literal character string or a name
#' @param value object, usually of a similar class as the dereferenced value of \code{x}, used for assigning in place
#' @param ... values passed to the function after dereferencing
#'
#' @return Object of class \code{"ref"}
#'
#' @name Extract
#' @rdname Extract
#'
#' @examples
#' x <- list(
#'   a = 1,
#'   b = "hello",
#'   "world"
#' )
#' ref_to_x <- ref(x)
#'
#' # Extract parts of 'x' from the reference
#' ref_to_x$a
#' ref_to_x[2:3]
#' ref_to_x[["b"]]
#'
#' # Replace parts of 'x' through the reference
#' ref_to_x[["a"]] <- 100
#' x$a == 100
#'
#' ref_to_x$b <- "bye"
#' x$b == "bye"
#'
#' ref_to_x[2:3] <- list(2, 3)
#' print(x)
#'
#'
#'
NULL

#' @rdname Extract
#' @export
`$.ref` <- function(x, name){
  dx <- deref(x)
  if (is.null(dx)){ stop("Reference is NULL.") }
  eval(as.call(list(`$`, dx, name)))
}
#' @rdname Extract
#' @export
`$.sref` <- function(x, ..., value){
  stop("Values cannot be extracted from sref objects.")
}
#' @rdname Extract
#' @export
`$<-.ref` <- function(x, name, value){
  if (is.nullref(x)){ stop("Reference is NULL.") }
  expr <- substitute(x$y <- value)
  expr[[2]][[2]] <- as.symbol(getSym(x))
  expr[[2]][[3]] <- name
  expr[[3]] <- value
  eval(expr, envir=getEnv(x))
  x
}
#' @rdname Extract
#' @export
`$<-.sref` <- function(x, ..., value){
  stop("Values cannot be set inside sref objects.")
}
#' @rdname Extract
#' @export
`[.ref` <- function(x, ...){
  dx <- deref(x)
  if (is.null(dx)){ stop("Reference is NULL.") }
  eval(as.call(list(`[`, dx, ...)))
}
#' @rdname Extract
#' @export
`[.sref` <- function(x, ..., value){
  stop("Values cannot be extracted from sref objects.")
}
#' @rdname Extract
#' @export
`[<-.ref` <- function(x, ..., value){
  if (is.nullref(x)){ stop("Reference is NULL.") }
  dots <- list(...)
  ndot <- length(dots)
  expr <- substitute(x[] <- value)
  expr[[2]][[2]] <- as.symbol(getSym(x))
  if (ndot >= 1){
    expr[[2]][3:(3+ndot-1)] <- dots
  }
  expr[[3]] <- value
  eval(expr, envir=getEnv(x))
  x
}
#' @rdname Extract
#' @export
`[<-.sref` <- function(x, ..., value){
  stop("Values cannot be set inside sref objects.")
}
#' @rdname Extract
#' @export
`[[.ref` <- function(x, ...) {
  dx <- deref(x)
  if (is.null(dx)){ stop("Reference is NULL.") }
  eval(as.call(list(`[[`, dx, ...)))
}
#' @rdname Extract
#' @export
`[[.sref` <- function(x, ..., value){
  stop("Values cannot be extracted from sref objects.")
}
#' @rdname Extract
#' @export
`[[<-.ref` <- function(x, ..., value){
  if (is.nullref(x)){ stop("Reference is NULL.") }
  dots <- list(...)
  ndot <- length(dots)
  expr <- substitute(x[[]] <- value)
  expr[[2]][[2]] <- as.symbol(getSym(x))
  if (ndot >= 1){
    expr[[2]][3:(3+ndot-1)] <- dots
  }
  expr[[3]] <- value
  eval(expr, envir=getEnv(x))
  x
}
#' @rdname Extract
#' @export
`[[<-.sref` <- function(x, ..., value){
  stop("Values cannot be set inside sref objects.")
}


