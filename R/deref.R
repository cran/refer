#' Dereference Object
#'
#' Return object from a \code{\link{ref}}. \code{`!`} can also be used to dereference an object.
#' See \code{\link{ref}} for more details.
#'
#' @param x reference object
#'
#' @details
#' \code{deref} is used to obtain the object originally referenced from \code{\link{ref}}.
#' \code{NULL} is returned if the object is no longer available. \code{ref} objects are
#' automatically dereferenced when using generic functions such as arithmetic operators.
#' Dereferencing a non-ref object just returns the object.
#'
#' @return R Obj or \code{NULL}
#'
#' @export
#' @examples
#' # Create a vectors of random numbers
#' x <- rnorm(10)
#' y <- runif(10)
#'
#' # Create a reference to the random numbers
#' ref_to_x <- ref(x)
#' ref_to_y <- ref(y)
#'
#' # Place references in a list
#' list_of_refs <- list(x = ref_to_x, y = ref_to_y)
#'
#' # Check sum of refs 'x' and 'y'
#' # Note that both `+` and `sum` automatically deref
#' sum1 <- sum(list_of_refs$x + list_of_refs$y)
#'
#' # Update 'x' and calculate new sum
#' x <- rnorm(10)
#' sum2 <- sum(list_of_refs$x + list_of_refs$y)
#'
#' # check diff in sums to see if 'list_of_refs' updated
#' sum2 - sum1
#'
#' # Obtain a reference to an expression
#' ref_to_part <- ref(x[2:5] + 3)
#' deref(ref_to_part)
#'
#' # Another expression reference
#' refs_to_list <- ref(list(x, y))
#' deref(refs_to_list)
#'
#' x <- "hello"
#' y <- "world"
#'
#' deref(refs_to_list)
#'
#' # Alternative, `!` can be used for dereferencing
#' !refs_to_list
#'
#' identical(!refs_to_list, deref(refs_to_list))
#'
#' # Referencing data.frame columns
#' dat <- data.frame(first = 1:4, second = 5:8)
#' ref_to_first <- ref(dat$first)
#' mean1 <- mean(!ref_to_first)
#'
#' dat$first <- dat$first * 4
#' mean2 <- mean(!ref_to_first)
#'
#' mean2 == 4*mean1
#'
#' # Many operations automatically dereference
#' ref_to_first * 5
#' ref_to_x == ref_to_y
#' cos(ref_to_first)
#' max(ref_to_first)
#'
deref <- function(x) UseMethod("deref")
#' @export
#' @method deref default
deref.default <- function(x) x
#' @export
#' @method deref ref
deref.ref <- function(x) {
  getEnv(x)[[getSym(x)]]
}
#' @export
#' @method deref sref
deref.sref <- function(x) {
  getEnv(x)[[getSym(x)]]
}
#' @export
#' @method deref slice
deref.slice <- function(x) {
  obj <- getEnv(x)[[getSym(x)]]
  do.call(`[`, append(list(obj), getIndex(x)))
}
#' @export
#' @method deref sslice
deref.sslice <- function(x) {
  obj <- getEnv(x)[[getSym(x)]]
  do.call(`[`, append(list(obj), getIndex(x)))
}
#' @export
#' @method deref rfexpr
deref.rfexpr <- function(x) {
  eval(x)
}

#' @export
#' @rdname deref
#' @method ! ref
`!.ref` <- function(x) deref(x)
#' @export
#' @method ! sref
`!.sref` <- function(x) deref(x)
#' @export
#' @method ! slice
`!.slice` <- function(x) deref(x)
#' @export
#' @method ! sslice
`!.sslice` <- function(x) deref(x)
#' @export
#' @method ! rfexpr
`!.rfexpr`  <- function(x) deref(x)
