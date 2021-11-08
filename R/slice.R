#' Create a Reference Slice to a Vector
#'
#' Create a reference to a 'part' of an R object. Use \code{\link{deref}} or \code{`!`} to obtain the values
#' within the referenced object.
#'
#' @param x object to be referenced; must be a symbol or character
#' @param ... objects passed to \code{x[...]} when dereferenced
#'
#' @return object of class \code{"slice"} and \code{"ref"}
#' @export
#'
#' @details
#' \code{slice} is similar to \code{\link{ref}}; it creates a reference to another R object. There are two
#' main differences with \code{ref}. First, \code{slice} only accepts names or characters instead of
#' expressions. Second, \code{slice} records a part of the underlying object. \code{slice(x, 1:2, 3)}
#' is equivalent to the reference of \code{x[1:2, 3]}. This is similar to \code{ref(x[1:2, 3])}, though the
#' implementation is different. \code{ref} would create an expression with a reference to \code{x}, while
#' \code{slice(x, 1:2, 3)} creates a list with a reference to \code{x} and the extract inputs. \code{slice}
#' is more efficient, but is limited in its capabilities.
#'
#' @examples
#' ## Vector Slice
#' x <- 10:1
#'
#' slice_x <- slice(x, 2:4)
#' identical(!slice_x, 9:7)   # TRUE
#'
#' x <- x - 2
#' identical(!slice_x, 7:5)   # TRUE
#'
#' ## Matrix Slice
#' y <- matrix(1:9, nrow=3)
#' slice_y <- slice(y, 2, 3)
#'
#' identical(!slice_y, y[2, 3])   # TRUE
slice <- function(x, ...){
  sub_x <- substitute(x)
  if (is.name(sub_x)){ sub_x <- as.character(sub_x)
  } else if (is.character(sub_x)){ sub_x <- sub_x[[1]]
  } else { stop("'slice' only accepts symbols or character strings.") }
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
      sub_x,
      list(...)
    ),
    class = c("slice", "ref")
  )
}
