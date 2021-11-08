#' Create Reference to an Object
#'
#' Create a reference to an arbitrary R object. Use \code{\link{deref}} or \code{`!`} to obtain the values
#' within the referenced object. Use \code{\link{sref}} to create a safer reference that limits modification
#' in place.
#'
#' @param x object to be referenced. \code{x} can be a symbol, character, or an expression containing a symbol.
#'
#' @details
#' Since R does not have reference semantics outside of environments, \code{ref} records the environment location
#' of an object rather than its memory address.\code{ref(x)} searches for object with name \code{"x"} within the
#' search path. If found, a reference to the environment and the name \code{"x"} are recorded. Otherwise, an
#' error is returned.
#'
#' \code{ref} can also create a reference to objects within an expression. \code{ref} searches the uncalled names
#' within the expression and replaces them with a reference to the object and a call to deref. For example,
#' \code{ref(x[[y]][2])} inserts a reference to variable \code{x} and variable \code{y} from the search path into
#' the expression then wraps the expression into an object of class \code{"ref_exp"}. These objects are
#' dereferenced by evaluating the expression. An error is returned only if the corresponding variables cannot
#' be found along the search path.
#'
#' \code{\link{deref}} can be used to find the objects at the referenced location. This usually results in a
#' copy of the objects. If the object is no longer available, \code{NULL} will be returned. Generic functions on
#' a \code{ref} object, such as arithmetic or \code{`sqrt`}, will automatically dereference the object before
#' applying the generic function. See \link{Methods} and \link{Extract} for a list of available functions
#' where explicit dereferencing is not needed. If this behavior is not desired, then \code{\link{sref}} can
#' be used to force the explicit use of \code{deref}.
#'
#' See \link{Extract} and \code{\link{modify_by}} for functions that modify the underlying value in place.
#'
#' An active binding could also be used instead of creating a reference. Active bindings, though, can be more
#' difficult to pass around and may have additional overhead since they are functions.
#'
#' \code{ref} can provide unsafe or inconsistent code that is susceptible to side-effects. Apply caution and
#' restraint with its use and be sure to \code{deref} before exporting any \code{ref} objects.
#'
#' @return a list of class \code{"ref"} containing a reference to the environment of the object and the name of
#' the object to be found within the environment, or an expression of class \code{"rfexpr"} containing references
#'
#' @export
#' @importFrom utils find
#'
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
ref <- function(x) {
  sub_x <- substitute(x)
  if (is.name(sub_x)){
    sub_x <- as.character(sub_x)
  } else if (is.character(sub_x)) {
    if (length(sub_x) != 1){ stop("Only character strings of length 1 can be used as a reference") }
  } else if (is.language(sub_x)) {
    n <- sys.nframe()
    new_ref <- ref_sub_lang_(sub_x, n-1)
    class(new_ref) <- "rfexpr"
    return(new_ref)
  } else {
    stop("Referenced object must be a symbol or character string of length 1.")
  }
  .env_obj <- NULL
  for (i in (sys.nframe()-1):0){
    if (!is.null(get0(sub_x, envir=sys.frame(i), ifnotfound = NULL))){
      .env_obj <- sys.frame(i)
      break
    }
  }
  if (is.null(.env_obj)){
    e <- find(sub_x, numeric = TRUE)
    if (length(e) == 0){ stop(paste0("Object '", sub_x, "' could not be found in the search path.")) }
    .env_obj <- pos.to.env(e)
  }
  structure (
    list (
      .env_obj,
      sub_x
    ),
    class = "ref"
  )
}
ref_sub_lang_ <- function(x, n) {
  if (is.name(x)){
    res <- substitute(deref(x))
    res[[2]] <- as_ref_(as.character(x), n)
    return(res)
  } else if (is.language(x)){
    nl <- length(x)
    if (nl > 1){
      if (identical(x[[1]], substitute(`$`)) || identical(x[[1]], substitute(`@`))) {
        x[[2]] <- ref_sub_lang_(x[[2]], n)
      } else {
        for (i in 1:nl){
          x[[i]] <- ref_sub_lang_(x[[i]], n)
        }
      }
      return(x)
    }
  }
  x
}
dederef_exp_ <- function(x){
  if (is.symbol(x)) return(x)
  if (is.language(x)){
    if (identical(x[[1]], substitute(deref))){
      return(as.symbol(getSym(x[[2]])))
    }
    for (i in 1:length(x)){
      x[[i]] <- dederef_exp_(x[[i]])
    }
  }
  x
}
#' @importFrom utils find
as_ref_ <- function(x, n){
  .env_obj <- NULL
  for (i in n:0){
    if (!is.null(get0(x, envir=sys.frame(i), ifnotfound = NULL, inherits=FALSE))){
      .env_obj <- sys.frame(i)
      break
    }
  }
  if (is.null(.env_obj)){
    e <- find(x, numeric = TRUE)
    if (length(e) == 0){ stop(paste0("Object '", x, "' could not be found in the search path.")) }
    .env_obj <- pos.to.env(e)
  }
  structure (
    list (
      .env_obj,
      x
    ),
    class = "ref"
  )
}

#' Create A List of References
#'
#' Create a list of references or referenced expressions. See \code{\link{ref}} for more details.
#'
#' @param ... objects to be referenced, possibly named.
#'
#' @return a list containing object references
#' @export
#'
#' @examples
#' x <- 1
#' y <- "hello"
#' z <- list(a = 1, b = 2, c = 3)
#'
#' new_list <- ref_list(x, second = y, z)
#'
#' !new_list[[1]]
#' (!new_list$second) == y  # TRUE
#'
#' y <- 18
#' (!new_list$second) == 18 # TRUE
#'
ref_list <- function(...) {
  dots <- eval(substitute(alist(...)))
  ndot <- length(dots)
  if (ndot == 0){ return(list()) }
  l <- vector("list", ndot)
  for (i in 1:ndot){
    l[[i]] <- do.call(ref, list(dots[[i]]))
  }
  names(l) <- names(dots)
  return(l)
}

