#' Create a Safer Reference to an Object
#'
#' Create a reference to an arbitrary R object. See \code{\link{ref}} for more details. \code{sref} behaves
#' similar to \code{ref}, but does not have support for direct operations on the referenced object.
#'
#' @param x object to be referenced. \code{x} can be a symbol, character, or an expression containing a symbol.
#'
#' @details
#' \code{sref} is similar to \code{\link{ref}}; it accepts either an R object or an expression, then records
#' its location. \code{ref} objects prioritize convenience, while \code{sref} objects prioritize clarity and
#' safety. For example, \code{`[`} and \code{`$`} can be used on a \code{ref} object to access the elements
#' of the underlying object, while \code{`[<-`} and \code{`$<-`} can be used to overwrite elements within.
#' These do not work for \code{sref} objects. Furthermore, base mathematical functions such as \code{`+`}
#' and \code{sqrt} also will not automatically dereference before applying.
#'
#' @export
#' @importFrom utils find
#'
#' @examples
#' x <- 1:10
#' ref_x  <- ref(x)
#' sref_x <- sref(x)
#'
#' ## These operations will run:
#' ref_x + 5
#' ref_x[1:4]
#' ref_x[7] <- 5
#'
#' ## These operations will not run:
#' # sref_x + 5
#' # sref_x[1:4]
#' # sref_x[7] <- 5
#'
sref <- function(x) {
  sub_x <- substitute(x)
  if (is.name(sub_x)){
    sub_x <- as.character(sub_x)
  } else if (is.character(sub_x)) {
    if (length(sub_x) != 1){ stop("Only character strings of length 1 can be used as a reference") }
  } else if (is.language(sub_x)) {
    n <- sys.nframe()
    new_ref <- sref_sub_lang_(sub_x, n-1)
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
    class = "sref"
  )
}
sref_sub_lang_ <- function(x, n) {
  if (is.name(x)){
    res <- substitute(deref(x))
    res[[2]] <- as_sref_(as.character(x), n)
    return(res)
  } else if (is.language(x)){
    nl <- length(x)
    if (nl > 1){
      if (identical(x[[1]], substitute(`$`)) || identical(x[[1]], substitute(`@`))) {
        x[[2]] <- sref_sub_lang_(x[[2]], n)
      } else {
        for (i in 1:nl){
          x[[i]] <- sref_sub_lang_(x[[i]], n)
        }
      }
      return(x)
    }
  }
  x
}
#' @importFrom utils find
as_sref_ <- function(x, n){
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
    class = "sref"
  )
}


#' Create a Safer Reference Slice to a Vector
#'
#' Create a reference to a 'part' of an R object. \code{sslice} behaves similar to \code{\link{slice}}, but does not
#' have support for direct operations on the referenced object. See \code{\link{sref}} for details about the behavior.
#'
#' @param x object to be referenced; must be a symbol or character
#' @param ... objects passed to \code{x[...]} when dereferenced
#'
#' @return object of class \code{"sslice"} and \code{"sref"}
#' @export
#'
sslice <- function(x, ...){
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
    class = c("sslice", "sref")
  )
}
