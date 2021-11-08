#' @importFrom utils capture.output
#' @method print ref
#' @export
print.ref <- function(x, ...){
  tryCatch(cat(paste0(capture.output(print(getEnv(x))), " => ", getSym(x))), error=function(e) cat("<NULL>"))
}

#' @importFrom utils capture.output
#' @method print slice
#' @export
print.slice <- function(x, ...){
  tryCatch(cat(paste0(capture.output(print(getEnv(x))), " => ", getSym(x), "[]")), error=function(e) cat("<NULL>"))
}

#' @importFrom utils capture.output
#' @method print ref
#' @export
print.sref <- function(x, ...){
  tryCatch(cat(paste0(capture.output(print(getEnv(x))), " => ", getSym(x))), error=function(e) cat("<NULL>"))
}

#' @importFrom utils capture.output
#' @method print rfexpr
#' @export
print.rfexpr <- function(x, ...){
  class(x) <- NULL
  cat(paste0("<", capture.output(print(dederef_exp_(x))), ">"))
}
