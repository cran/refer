#' Increment Value In Place
#'
#' Increase the value of an object on the search path. Equivalent to \code{x++} or \code{x += by} in other languages.
#'
#' @param x object to be incremented; can be a symbol, character, or extraction language object.
#' @param by value to increase \code{x} by; defaults to \code{1}.
#'
#' @details
#' \code{incr} quotes object \code{x}, then attempts to determine the primary object to be modified.
#' For example, \code{z} will be the 'primary object' in \code{incr(z[1:4])}. \code{incr} then searches
#' for the primary object in the search path and records the environment. \code{x <- x + by} is then
#' evaluated within the recorded environment.
#'
#' The quoted object can be a symbol or character object. It can also be language object, though the primary
#' call must be either \code{`$`}, \code{`[`}, or \code{`[[`}. These can be nested. For example, \code{x[1]}
#' or \code{x[2, 1][3]} is acceptable, but \code{sqrt(x)} is not.
#'
#' See \code{\link{decr}} to decrease the value.
#'
#' @return the value of \code{x} incremented by \code{by}, invisibly
#' @export
#'
#' @examples
#' z <- 1:10
#'
#' incr(z)
#' identical(z, as.numeric(2:11))       # TRUE
#'
#' incr(z[1:3], by=2)
#' identical(z[1:3], as.numeric(4:6))   # TRUE
#'
#' l <- list(a = 1, b = 2)
#' decr(l$a)
#' l$a == 0   # TRUE
#'
#' decr(l$b, by = 4)
#' l$b == -2  # TRUE
incr <- function(x, by = 1){
  modify_by_(x, by, substitute(x <- x + y), sys.nframe()-1)
}
#' Decrease Value In Place
#'
#' Decrease the value of an object on the search path. Equivalent to \code{x--} or
#' \code{x -= by} in other languages. See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be decreased; can be a symbol, character, or extraction language object.
#' @param by value to decrease \code{x} by; defaults to \code{1}.
#'
#' @return the value of \code{x} decreased by \code{by}, invisibly
#' @export
#'
#' @examples
#' z <- 1:10
#'
#' incr(z)
#' identical(z, 2:11)       # TRUE
#'
#' incr(z[1:3], by=2)
#' identical(z[1:3], 4:6)   # TRUE
#'
#' l <- list(a = 1, b = 2)
#' decr(l$a)
#' l$a == 0   # TRUE
#'
#' decr(l$b, by = 4)
#' l$b == -2  # TRUE
decr <- function(x, by = 1){
  modify_by_(x, by, substitute(x <- x - y), sys.nframe()-1)
}


#' Add In Place
#'
#' Increase the value of an object on the search path. Equivalent to \code{'+='} in other languages.
#' See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be modified; can be a symbol, character, or extraction language object.
#' @param value value with which to change \code{x} by
#'
#' @return the new value of \code{x}, invisibly
#' @export
#'
#' @examples
#' x <- 1:10
#' x %+=% 10
#' identical(x, 11:20)  # TRUE
`%+=%` <- function(x, value){
  modify_by_(x, value, substitute(x <- x + y), sys.nframe()-1)
}

#' Subtract In Place
#'
#' Decrease the value of an object on the search path. Equivalent to \code{'-='} in other languages.
#' See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be modified; can be a symbol, character, or extraction language object.
#' @param value value with which to change \code{x} by
#'
#' @return the new value of \code{x}, invisibly
#' @export
#'
#' @examples
#' x <- 11:20
#' x %-=% 10
#' identical(x, 1:10)  # TRUE
`%-=%` <- function(x, value){
  modify_by_(x, value, substitute(x <- x - y), sys.nframe()-1)
}

#' Multiply In Place
#'
#' Change the value of an object on the search path through multiplication. Equivalent to \code{'*='} in other languages.
#' See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be modified; can be a symbol, character, or extraction language object.
#' @param value value with which to change \code{x} by
#'
#' @return the new value of \code{x}, invisibly
#' @export
#'
#' @examples
#' x <- 5
#' x %*=% 2
#' identical(x, 10)  # TRUE
`%*=%` <- function(x, value){
  modify_by_(x, value, substitute(x <- x * y), sys.nframe()-1)
}

#' Divide In Place
#'
#' Change the value of an object on the search path through division. Equivalent to \code{'/='} in other languages.
#' See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be modified; can be a symbol, character, or extraction language object.
#' @param value value with which to change \code{x} by
#'
#' @return the new value of \code{x}, invisibly
#' @export
#'
#' @examples
#' x <- 10
#' x %/=% 2
#' identical(x, 5)  # TRUE
`%/=%` <- function(x, value){
  modify_by_(x, value, substitute(x <- x / y), sys.nframe()-1)
}

#' Power In Place
#'
#' Change the value of an object on the search path through exponentiation Equivalent to \code{'^='} in other languages.
#' See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be modified; can be a symbol, character, or extraction language object.
#' @param value value with which to change \code{x} by
#'
#' @return the new value of \code{x}, invisibly
#' @export
#'
#' @examples
#' x <- 10
#' x %^=% 2
#' identical(x, 100)  # TRUE
`%^=%` <- function(x, value){
  modify_by_(x, value, substitute(x <- x ^ y), sys.nframe()-1)
}

#' Matrix Multiplication In Place
#'
#' Change the value of an object on the search path through matrix multiplication. Similar to \code{'*='} in
#' other languages, except with matrix multiplication. See \code{\link{incr}} for details on implementation.
#'
#' @param x object to be modified; can be a symbol, character, or extraction language object.
#' @param value value with which to change \code{x} by
#'
#' @return the new value of \code{x}, invisibly
#' @export
#'
#' @examples
#' x <- 1:5
#' x %*=% 6:10
#' identical(x, 130)  # TRUE
`%.*=%` <- function(x, value){
  modify_by_(x, value, substitute(x <- x %*% y), sys.nframe()-1)
}


modify_by_ <- function(x, value, fun, n){
  fun[[3]][[3]] <- value
  if (is.slice(x)){
    s <- substitute(x[])
    s[[2]] <- as.symbol(getSym(x))
    ind <- getIndex(x)
    if (length(ind)>0){
      for (i in 1:length(ind)){
        s[[2+i]] <- ind[[i]]
      }
    }
    e <- getEnv(x)
  } else if (is.ref(x)){
    s <- as.symbol(getSym(x))
    e <- getEnv(x)
  } else {
    s <- substitute(x, sys.frame(-1))
    e <- find_obj_(s, n)
  }
  fun[[2]] <- s
  fun[[3]][[2]] <- s
  eval(fun, e)
}

find_obj_ <- function(x, n){
  if (is.symbol(x) || is.character(x)){
    sub_x <- as.character(x)[[1]]
    for (i in n:0){
      if (!is.null(get0(sub_x, envir=sys.frame(i), ifnotfound = NULL))){
        return(sys.frame(i))
      }
    }
    stop("Object could not be found in the search path.")
  } else if (is.language(x)){
    call_fun <- x[[1]]
    if (
      identical(call_fun, substitute(`$`)) ||
      identical(call_fun, substitute(`[`)) ||
      identical(call_fun, substitute(`[[`))
    ) { return(find_obj_(x[[2]], n)) }
  }
  stop("Object location could not be determined. Only symbols or extraction expressions may be used.")
}
