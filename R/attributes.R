#' Extract or Set Reference Environment
#'
#' Functions to obtain or set the environment to which a \code{\link{ref}} or \code{\link{sref}} object points.
#'
#' @param x object of class \code{"ref"} or \code{"sref"}
#' @param e new environment to which the reference points
#'
#' @return environment for \code{getEnv} or reference object for \code{setEnv}
#'
#' @export
#'
#' @examples
#' x <- 1:10
#' ref_to_x <- ref(x)
#' ref_env  <- getEnv(ref_to_x)
#' ref_sym  <- getSym(ref_to_x)
#'
#' identical(ref_env, .GlobalEnv)
#' identical(ref_sym, "x")
#'
#' e <- new.env()
#' e$x <- 100
#' ref_to_x <- setEnv(ref_to_x, e)
#' !ref_to_x
#'
getEnv <- function(x) UseMethod("getEnv")
#' @export
#' @method getEnv ref
getEnv.ref <- function(x){
  class(x) <- "list"
  x[[1]]
}
#' @export
#' @method getEnv sref
getEnv.sref <- function(x){
  class(x) <- "list"
  x[[1]]
}
#' @export
#' @method getEnv slice
getEnv.slice <- function(x){
  class(x) <- "list"
  x[[1]]
}

#' @export
#' @rdname getEnv
setEnv <- function(x, e) UseMethod("setEnv")
#' @export
#' @method setEnv ref
setEnv.ref <- function(x, e) {
  if (!is.environment(e)) stop("Reference must be set with an environment.")
  class_holder <- class(x)
  class(x) <- "list"
  x[[1]] <- e
  class(x) <- class_holder
  x
}
#' @export
#' @method setEnv sref
setEnv.sref <- function(x, e) {
  if (!is.environment(e)) stop("Reference must be set with an environment.")
  class_holder <- class(x)
  class(x) <- "list"
  x[[1]] <- e
  class(x) <- class_holder
  x
}
#' @export
#' @method setEnv slice
setEnv.slice <- function(x, e) {
  if (!is.environment(e)) stop("Reference must be set with an environment.")
  class_holder <- class(x)
  class(x) <- "list"
  x[[1]] <- e
  class(x) <- class_holder
  x
}



#' Extract or Set Reference Symbol
#'
#' Functions to obtain or set the object name to which a \code{\link{ref}} or \code{\link{sref}} object points.
#'
#' @param x object of class \code{"ref"}
#' @param sym symbol or character naming the object to which the reference points
#'
#' @return character of length 1
#'
#' @export
#'
#' @examples
#' x <- 1:10
#' ref_to_x <- ref(x)
#' ref_env  <- getEnv(ref_to_x)
#' ref_sym  <- getSym(ref_to_x)
#'
#' identical(ref_env, .GlobalEnv)
#' identical(ref_sym, "x")
#'
#' y <- 500
#' ref_to_x <- setSym(ref_to_x, y)
#' !ref_to_x
#'
#' @export
getSym <- function(x) UseMethod("getSym")
#' @export
#' @method getSym ref
getSym.ref <- function(x){
  class(x) <- "list"
  x[[2]]
}
#' @export
#' @method getSym sref
getSym.sref <- function(x){
  class(x) <- "list"
  x[[2]]
}
#' @export
#' @method getSym slice
getSym.slice <- function(x){
  class(x) <- "list"
  x[[2]]
}

#' @export
#' @rdname getSym
setSym <- function(x, sym) UseMethod("setSym")
#' @export
#' @method setSym ref
setSym.ref <- function(x, sym){
  class_holder <- class(x)
  class(x) <- "list"
  x[[2]] <- as.character(x)[[1]]
  class(x) <- class_holder
  x
}
#' @export
#' @method setSym sref
setSym.sref <- function(x, sym){
  class_holder <- class(x)
  class(x) <- "list"
  x[[2]] <- as.character(x)[[1]]
  class(x) <- class_holder
  x
}
#' @export
#' @method setSym slice
setSym.slice <- function(x, sym){
  class_holder <- class(x)
  class(x) <- "list"
  x[[2]] <- as.character(x)[[1]]
  class(x) <- class_holder
  x
}


#' Extract or Set Slice Index
#'
#' Functions to obtain or set the index to which a \code{\link{slice}} object points.
#'
#' @param x object of class \code{"slice"}
#' @param ... objects compatible with extracting or replacing a vector
#'
#' @return object of class \code{"slice"}
#'
#' @export
#'
#' @examples
#' x <- matrix(1:9, nrow=3)
#' slice_x <- slice(x, 2:3, 1)
#' identical(getIndex(slice_x), list(2:3, 1)) # TRUE
#'
#' setIndex(slice_x, list(1, substitute()))
#' identical(!slice_x, c(1, 4, 7))    # TRUE
#'
#' @export
getIndex <- function(x) UseMethod("getIndex")
#' @export
#' @method getIndex slice
getIndex.slice <- function(x){
  class(x) <- "list"
  x[[3]]
}
#' @export
#' @method getIndex sslice
getIndex.sslice <- function(x){
  class(x) <- "list"
  x[[3]]
}

#' @export
#' @rdname getIndex
setIndex <- function(x, ...) UseMethod("setIndex")
#' @export
#' @method setIndex slice
setIndex.slice <- function(x, ...){
  class_holder <- class(x)
  class(x) <- "list"
  x[[3]] <- list(...)
  class(x) <- class_holder
  x
}
#' @export
#' @method setIndex sslice
setIndex.sslice <- function(x, ...){
  class_holder <- class(x)
  class(x) <- "list"
  x[[3]] <- list(...)
  class(x) <- class_holder
  x
}

