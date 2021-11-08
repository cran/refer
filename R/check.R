#' Is Object a Reference?
#'
#' Check whether an R Object inherits a reference class.
#'
#' @param x object of any class
#'
#' @return \code{TRUE} if \code{x} is a reference object, otherwise \code{FALSE}
#'
#' @export
#'
#' @examples
#' # Create a vectors of random numbers
#' x <- rnorm(10)
#'
#' # Create a reference to the random numbers
#' ref_to_x <- ref(x)
#'
#' is.ref(ref_to_x) # TRUE
#'
is.ref <- function(x) inherits(x, "ref")
#' @export
#' @describeIn is.ref check whether object is an 'sref' object
is.sref   <- function(x) inherits(x, "sref")
#' @export
#' @describeIn is.ref check whether object is a reference expression
is.rfexpr <- function(x) inherits(x, "rfexpr")
#' @export
#' @describeIn is.ref check whether object references a slice of a vector
is.slice <- function(x) inherits(x, "slice")
#' @export
#' @describeIn is.ref check whether object is any type of reference class
is.a.ref  <- function(x) any(c("ref", "sref", "rfexpr", "slice") %in% class(x))


#' Is Reference Null?
#'
#' Check whether a \code{\link{ref}} points to a \code{NULL} object or an object
#' that no longer exists.
#'
#' @param x object of class \code{"ref"}
#'
#' @return \code{TRUE} if \code{x} is not a reference or points to an object that does not exist; otherwise \code{FALSE}.
#'
#' @export
#'
#' @examples
#' # Create a vectors of random numbers and a reference
#' x <- rnorm(10)
#' ref_to_x <- ref(x)
#'
#' # Delete 'x' and check if NULL
#' is.nullref(ref_to_x) # FALSE
#' rm(x)
#' is.nullref(ref_to_x) # TRUE
#'
is.nullref <- function(x) UseMethod("is.nullref")
#' @export
#' @method is.nullref default
is.nullref.default <- function(x) FALSE
#' @export
#' @method is.nullref ref
is.nullref.ref <- function(x) tryCatch(is.null(deref(x)), error = function(e) TRUE)
#' @export
#' @method is.nullref sref
is.nullref.sref <- function(x) tryCatch(is.null(deref(x)), error = function(e) TRUE)

