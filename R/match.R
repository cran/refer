#' Check and Evaluate Match Condition
#'
#' \code{\link{ref}} methods for use with \code{\link[matchr]{Match}} in the \code{matchr} package.
#'
#' @param cond match condition
#' @param x object being matched
#' @param do return expression associated with the condition. If \code{cond} is matched with \code{x}, then \code{do}
#' should be evaluated and returned in a list with \code{TRUE}: \code{list(TRUE, eval(do))}.
#' @param ... arguments passed to evaluation
#'
#' @details
#' See \code{\link[matchr]{Match}} for details about the implementation of \code{match_cond}. When matching,
#' \code{ref} conditions check whether \code{x} is a \code{ref} object. If so, then a match occurs if the condition
#' and \code{x} point to the same object. Otherwise, the condition is dereferenced and the resulting value
#' is checked using the appropriate match condition. Note that a \code{\link{slice}} is never matched with a
#' \code{\link{ref}} and vice versa, though \code{\link{ref}} and \code{\link{sref}} objects may match if they
#' point to the same object.
#'
#' @return \code{FALSE} if no match, or a list containing \code{TRUE} and the evaluated expression
#' @importFrom matchr match_cond
#' @name match_cond.ref
#' @rdname match_cond.ref
#' @export
#'
#' @examples
#' x <- 1:10
#' ref_to_x <- ref(x)
#'
#' matchr::Match(
#'   x,
#'   is.character -> "is a character",
#'   ref_to_x     -> "same as reference",   # <- MATCH
#'   .            -> "anything else"
#' )
#'
NULL

#' @rdname match_cond.ref
#' @method match_cond ref
#' @importFrom matchr match_cond
#' @export
match_cond.ref <- function(cond, x, do, ...){
  if (is.slice(x)) return(FALSE)
  if (is.ref(x) || is.sref(x)){
    if (identical(getEnv(cond), getEnv(x)) && identical(getSym(cond), getSym(x))){
      return(eval_match(do, ...))
    }
    return(FALSE)
  }
  cond2 <- deref(cond)
  match_cond(cond2, x, do, ...)
}
#' @rdname match_cond.ref
#' @method match_cond sref
#' @importFrom matchr match_cond
#' @export
match_cond.sref <- function(cond, x, do, ...){
  if (is.slice(x)) return(FALSE)
  if (is.ref(x) || is.sref(x)){
    if (identical(getEnv(cond), getEnv(x)) && identical(getSym(cond), getSym(x))){
      return(eval_match(do, ...))
    }
    return(FALSE)
  }
  cond2 <- deref(cond)
  match_cond(cond2, x, do, ...)
}
#' @rdname match_cond.ref
#' @method match_cond slice
#' @importFrom matchr match_cond
#' @export
match_cond.slice <- function(cond, x, do, ...){
  if (is.slice(x)){
    if (identical(getEnv(cond), getEnv(x)) && identical(getSym(cond), getSym(x)) && identical(getIndex(cond), getIndex(x))){
      return(eval_match(do, ...))
    }
    return(FALSE)
  }
  cond2 <- deref(cond)
  match_cond(cond2, x, do, ...)
}
#' @rdname match_cond.ref
#' @method match_cond rfexpr
#' @importFrom matchr match_cond
#' @export
match_cond.rfexpr <- function(cond, x, do, ...){
  cond2 <- deref(cond)
  if (is.rfexpr(x)){ x <- deref(x) }
  match_cond(cond2, x, do, ...)
}


eval_match <- function(do, ...) {
  x <- eval(do, ...)
  if ("fallthrough" %in% class(x)){ return(FALSE) }
  list(TRUE, x)
}






#' Convert Reference to Iterable Object
#'
#' \code{\link{ref}} methods for use with \code{\link[eList]{iter}} in the \code{eList} package.
#' It allows \code{ref} objects to be used with the different vector comprehensions in the package
#' and with functions such as \code{\link[base]{lapply}} in base R.
#'
#' @param x object to be looped across
#'
#' @return a vector
#' @export
#' @importFrom eList iter
#' @rdname iter.ref
#' @name iter.ref
#' @examples
#' x <- sample(1:10, 5, replace=TRUE)
#' slice_x <- slice(x, 1:2)
#'
#' lapply(eList::iter(slice_x), print)
NULL

#' @rdname iter.ref
#' @method iter ref
#' @importFrom eList iter
#' @export
iter.ref <- function(x) iter(deref(x))
#' @rdname iter.ref
#' @method iter slice
#' @importFrom eList iter
#' @export
iter.slice <- function(x) iter(deref(x))
#' @rdname iter.ref
#' @method iter rfexpr
#' @importFrom eList iter
#' @export
iter.rfexpr <- function(x) iter(deref(x))
