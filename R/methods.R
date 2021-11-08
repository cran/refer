#' S3 Methods for Automatic Dereferencing
#'
#' These functions automatically call \code{\link{deref}} when applied to a \code{\link{ref}} or \code{"rfexpr"}
#' object. Therefore, there is no need to explicitly call \code{deref}. \code{\link{sref}} objects will need
#' to be explicitly dereferenced before applying these functions. All functions are from \code{base} R.
#'
#' @param x,y,e1,e2,z,target,current,object,a,b,formula objects of class \code{"ref"}
#' @param incomparables,digits,tz,row.names,optional,decreasing,na.rm,parm,level function specific arguments. See the relevant
#' functions for more details
#' @param ... other objects passed to the function
#'
#' @return An R object depending on the function.
#'
#' @name Methods
#' @rdname Methods
NULL


#' @export
#' @method Math ref
#' @rdname Methods
Math.ref <- function(x, ...) {
  z <- deref(x)
  generic <- get(.Generic, mode="function")
  generic(z, ...)
}
#' @export
#' @method Ops ref
#' @rdname Methods
Ops.ref <- function(e1, e2) {
  if (inherits(e1, "ref")) { e1 <- deref(e1) }
  if (inherits(e2, "ref")) { e2 <- deref(e2) }
  generic <- get(.Generic, mode="function")
  generic(e1, e2)
}
#' @export
#' @method Complex ref
#' @rdname Methods
Complex.ref <- function(z) {
  x <- deref(z)
  generic <- get(.Generic, mode="function")
  generic(x)
}
#' @export
#' @method Summary ref
#' @rdname Methods
Summary.ref <- function(..., na.rm=FALSE) {
  dots <- list(...)
  ndot <- length(dots)
  l <- vector("list", ndot+1)
  for (i in 1:ndot) l[[i]] <- deref(dots[[i]])
  l[["na.rm"]] <- na.rm
  do.call(.Generic, l)
}

#' @export
#' @method Math rfexpr
#' @rdname Methods
Math.rfexpr <- function(x, ...) {
  z <- deref(x)
  generic <- get(.Generic, mode="function")
  generic(z, ...)
}
#' @export
#' @method Ops rfexpr
#' @rdname Methods
Ops.rfexpr <- function(e1, e2) {
  if (inherits(e1, "rfexpr")) { e1 <- deref(e1) }
  if (inherits(e2, "rfexpr")) { e2 <- deref(e2) }
  generic <- get(.Generic, mode="function")
  generic(e1, e2)
}
#' @export
#' @method Complex rfexpr
#' @rdname Methods
Complex.rfexpr <- function(z) {
  x <- deref(z)
  generic <- get(.Generic, mode="function")
  generic(x)
}
#' @export
#' @method Summary rfexpr
#' @rdname Methods
Summary.rfexpr <- function(..., na.rm=FALSE) {
  dots <- list(...)
  ndot <- length(dots)
  l <- vector("list", ndot+1)
  for (i in 1:ndot) l[[i]] <- deref(dots[[i]])
  l[["na.rm"]] <- na.rm
  do.call(.Generic, l)
}



#' @export
#' @method all.equal ref
#' @rdname Methods
all.equal.ref <- function(target, current, ...) all.equal(deref(target), deref(current), ...)

#' @export
#' @method anyDuplicated ref
#' @rdname Methods
anyDuplicated.ref <- function(x, incomparables = FALSE, ...) anyDuplicated(deref(x), incomparables, ...)

#' @export
#' @method as.character ref
#' @rdname Methods
as.character.ref <- function(x, ...) as.character(deref(x), ...)

#' @export
#' @method as.data.frame ref
#' @rdname Methods
as.data.frame.ref <- function(x, row.names=NULL, optional=FALSE, ...) as.data.frame(deref(x), row.names, optional, ...)

#' @export
#' @method as.Date ref
#' @rdname Methods
as.Date.ref <- function(x, ...) as.Date(deref(x), ...)

#' @export
#' @method as.double ref
#' @rdname Methods
as.double.ref <- function(x, ...) as.double(deref(x), ...)

#' @export
#' @method as.function ref
#' @rdname Methods
as.function.ref <- function(x, ...) as.function(deref(x), ...)

#' @export
#' @method as.list ref
#' @rdname Methods
as.list.ref <- function(x, ...) as.list(deref(x), ...)

#' @export
#' @method as.matrix ref
#' @rdname Methods
as.matrix.ref <- function(x, ...) as.matrix(deref(x), ...)

#' @export
#' @method as.POSIXct ref
#' @rdname Methods
as.POSIXct.ref <- function(x, tz="", ...) as.POSIXct(deref(x), tz, ...)

#' @export
#' @method as.POSIXlt ref
#' @rdname Methods
as.POSIXlt.ref <- function(x, tz="", ...) as.POSIXlt(deref(x), tz, ...)

#' @export
#' @method as.single ref
#' @rdname Methods
as.single.ref <- function(x, ...) as.single(deref(x), ...)

#' @export
#' @method as.table ref
#' @rdname Methods
as.table.ref <- function(x, ...) as.table(deref(x), ...)

#' @export
#' @method c ref
#' @rdname Methods
c.ref <- function(...){
  dots <- list(...)
  ndot <- length(dots)
  l <- vector("list", ndot)
  for (i in 1:ndot){ l[[i]] <- deref(dots[[i]]) }
  names(l) <- names(dots)
  do.call(c, l)
}

#' @export
#' @method cut ref
#' @rdname Methods
cut.ref <- function(x, ...) cut(deref(x), ...)

#' @export
#' @method diff ref
#' @rdname Methods
diff.ref <- function(x, ...) diff(deref(x), ...)

#' @export
#' @method dim ref
#' @rdname Methods
dim.ref <- function(x) dim(deref(x))

#' @export
#' @method droplevels ref
#' @rdname Methods
droplevels.ref <- function(x, ...) droplevels(deref(x), ...)

#' @export
#' @method duplicated ref
#' @rdname Methods
duplicated.ref <- function(x, incomparables = FALSE, ...) duplicated(deref(x), incomparables, ...)

#' @export
#' @method format ref
#' @rdname Methods
format.ref <- function(x, ...) format(deref(x), ...)

#' @export
#' @method isSymmetric ref
#' @rdname Methods
isSymmetric.ref <- function(object, ...) isSymmetric(deref(object), ...)

#' @export
#' @method kappa ref
#' @rdname Methods
kappa.ref <- function(z, ...) kappa(deref(z), ...)

#' @export
#' @method labels ref
#' @rdname Methods
labels.ref <- function(object, ...) labels(deref(object), ...)

#' @export
#' @method length ref
#' @rdname Methods
length.ref <- function(x) length(deref(x))

#' @export
#' @method levels ref
#' @rdname Methods
levels.ref <- function(x) levels(deref(x))

#' @export
#' @method mean ref
#' @rdname Methods
mean.ref <- function(x, ...) mean(deref(x), ...)

#' @export
#' @method merge ref
#' @rdname Methods
merge.ref <- function(x, y, ...) merge(deref(x), deref(y), ...)

#' @export
#' @method qr ref
#' @rdname Methods
qr.ref <- function(x, ...) qr(deref(x), ...)

#' @export
#' @method rep ref
#' @rdname Methods
rep.ref <- function(x, ...) rep(deref(x), ...)

#' @export
#' @method rev ref
#' @rdname Methods
rev.ref <- function(x) rev(deref(x))

#' @export
#' @method round ref
#' @rdname Methods
round.ref <- function(x, digits=0) round(deref(x), digits=deref(digits))

#' @export
#' @method row.names ref
#' @rdname Methods
row.names.ref <- function(x) row.names(deref(x))

#' @export
#' @method solve ref
#' @rdname Methods
solve.ref <- function(a, b, ...) solve(deref(a), deref(b), ...)

#' @export
#' @method sort ref
#' @rdname Methods
sort.ref <- function(x, decreasing = FALSE, ...) sort(deref(x), decreasing, ...)

#' @export
#' @method aggregate ref
#' @importFrom stats aggregate
#' @rdname Methods
aggregate.ref <- function(x, ...) aggregate(deref(x), ...)

#' @export
#' @method coef ref
#' @importFrom stats coef
#' @rdname Methods
coef.ref <- function(object, ...) coef(deref(object), ...)

#' @export
#' @method confint ref
#' @importFrom stats confint
#' @rdname Methods
confint.ref <- function(object, parm, level=0.95, ...) confint(deref(object), parm, level=level, ...)

#' @export
#' @method fitted ref
#' @importFrom stats fitted
#' @rdname Methods
fitted.ref <- function(object, ...) fitted(deref(object), ...)

#' @export
#' @method median ref
#' @importFrom stats median
#' @rdname Methods
median.ref <- function(x, na.rm = FALSE, ...) median(deref(x), na.rm = na.rm, ...)

#' @export
#' @method model.frame ref
#' @importFrom stats model.frame
#' @rdname Methods
model.frame.ref <- function(formula, ...) model.frame(deref(formula), ...)

#' @export
#' @method model.matrix ref
#' @importFrom stats model.matrix
#' @rdname Methods
model.matrix.ref <- function(object, ...) model.matrix(deref(object), ...)

#' @export
#' @method na.omit ref
#' @importFrom stats na.omit
#' @rdname Methods
na.omit.ref <- function(object, ...) na.omit(deref(object), ...)

#' @export
#' @method plot ref
#' @rdname Methods
plot.ref <- function(x, y, ...) plot(deref(x), deref(y), ...)

#' @export
#' @method predict ref
#' @importFrom stats predict
#' @rdname Methods
predict.ref <- function(object, ...) predict(deref(object), ...)

#' @export
#' @method residuals ref
#' @importFrom stats residuals
#' @rdname Methods
residuals.ref <- function(object, ...) residuals(deref(object), ...)

#' @export
#' @method summary ref
#' @rdname Methods
summary.ref <- function(object, ...) summary(deref(object), ...)

#' @export
#' @method terms ref
#' @importFrom stats terms
#' @rdname Methods
terms.ref <- function(x, ...) terms(deref(x), ...)

#' @export
#' @method vcov ref
#' @importFrom stats vcov
#' @rdname Methods
vcov.ref <- function(object, ...) vcov(deref(object), ...)

#' @export
#' @method window ref
#' @importFrom stats window
#' @rdname Methods
window.ref <- function(x, ...) window(deref(x), ...)


#' @export
#' @method all.equal rfexpr
#' @rdname Methods
all.equal.rfexpr <- function(target, current, ...) all.equal(deref(target), deref(current), ...)

#' @export
#' @method anyDuplicated rfexpr
#' @rdname Methods
anyDuplicated.rfexpr <- function(x, incomparables = FALSE, ...) anyDuplicated(deref(x), incomparables, ...)

#' @export
#' @method as.character rfexpr
#' @rdname Methods
as.character.rfexpr <- function(x, ...) as.character(deref(x), ...)

#' @export
#' @method as.data.frame rfexpr
#' @rdname Methods
as.data.frame.rfexpr <- function(x, row.names=NULL, optional=FALSE, ...) as.data.frame(deref(x), row.names, optional, ...)

#' @export
#' @method as.Date rfexpr
#' @rdname Methods
as.Date.rfexpr <- function(x, ...) as.Date(deref(x), ...)

#' @export
#' @method as.double rfexpr
#' @rdname Methods
as.double.rfexpr <- function(x, ...) as.double(deref(x), ...)

#' @export
#' @method as.function rfexpr
#' @rdname Methods
as.function.rfexpr <- function(x, ...) as.function(deref(x), ...)

#' @export
#' @method as.list rfexpr
#' @rdname Methods
as.list.rfexpr <- function(x, ...) as.list(deref(x), ...)

#' @export
#' @method as.matrix rfexpr
#' @rdname Methods
as.matrix.rfexpr <- function(x, ...) as.matrix(deref(x), ...)

#' @export
#' @method as.POSIXct rfexpr
#' @rdname Methods
as.POSIXct.rfexpr <- function(x, tz="", ...) as.POSIXct(deref(x), tz, ...)

#' @export
#' @method as.POSIXlt rfexpr
#' @rdname Methods
as.POSIXlt.rfexpr <- function(x, tz="", ...) as.POSIXlt(deref(x), tz, ...)

#' @export
#' @method as.single rfexpr
#' @rdname Methods
as.single.rfexpr <- function(x, ...) as.single(deref(x), ...)

#' @export
#' @method as.table rfexpr
#' @rdname Methods
as.table.rfexpr <- function(x, ...) as.table(deref(x), ...)

#' @export
#' @method c rfexpr
#' @rdname Methods
c.rfexpr <- function(...){
  dots <- list(...)
  ndot <- length(dots)
  l <- vector("list", ndot)
  for (i in 1:ndot){ l[[i]] <- deref(dots[[i]]) }
  names(l) <- names(dots)
  do.call(c, l)
}

#' @export
#' @method cut rfexpr
#' @rdname Methods
cut.rfexpr <- function(x, ...) cut(deref(x), ...)

#' @export
#' @method diff rfexpr
#' @rdname Methods
diff.rfexpr <- function(x, ...) diff(deref(x), ...)

#' @export
#' @method dim rfexpr
#' @rdname Methods
dim.rfexpr <- function(x) dim(deref(x))

#' @export
#' @method droplevels rfexpr
#' @rdname Methods
droplevels.rfexpr <- function(x, ...) droplevels(deref(x), ...)

#' @export
#' @method duplicated rfexpr
#' @rdname Methods
duplicated.rfexpr <- function(x, incomparables = FALSE, ...) duplicated(deref(x), incomparables, ...)

#' @export
#' @method format rfexpr
#' @rdname Methods
format.rfexpr <- function(x, ...) format(deref(x), ...)

#' @export
#' @method isSymmetric rfexpr
#' @rdname Methods
isSymmetric.rfexpr <- function(object, ...) isSymmetric(deref(object), ...)

#' @export
#' @method kappa rfexpr
#' @rdname Methods
kappa.rfexpr <- function(z, ...) kappa(deref(z), ...)

#' @export
#' @method labels rfexpr
#' @rdname Methods
labels.rfexpr <- function(object, ...) labels(deref(object), ...)

#' @export
#' @method length rfexpr
#' @rdname Methods
length.rfexpr <- function(x) length(deref(x))

#' @export
#' @method levels rfexpr
#' @rdname Methods
levels.rfexpr <- function(x) levels(deref(x))

#' @export
#' @method mean rfexpr
#' @rdname Methods
mean.rfexpr <- function(x, ...) mean(deref(x), ...)

#' @export
#' @method merge rfexpr
#' @rdname Methods
merge.rfexpr <- function(x, y, ...) merge(deref(x), deref(y), ...)

#' @export
#' @method qr rfexpr
#' @rdname Methods
qr.rfexpr <- function(x, ...) qr(deref(x), ...)

#' @export
#' @method rep rfexpr
#' @rdname Methods
rep.rfexpr <- function(x, ...) rep(deref(x), ...)

#' @export
#' @method rev rfexpr
#' @rdname Methods
rev.rfexpr <- function(x) rev(deref(x))

#' @export
#' @method round rfexpr
#' @rdname Methods
round.rfexpr <- function(x, digits=0) round(deref(x), digits=deref(digits))

#' @export
#' @method row.names rfexpr
#' @rdname Methods
row.names.rfexpr <- function(x) row.names(deref(x))

#' @export
#' @method solve rfexpr
#' @rdname Methods
solve.rfexpr <- function(a, b, ...) solve(deref(a), deref(b), ...)

#' @export
#' @method sort rfexpr
#' @rdname Methods
sort.rfexpr <- function(x, decreasing = FALSE, ...) sort(deref(x), decreasing, ...)


#' @export
#' @method aggregate rfexpr
#' @importFrom stats aggregate
#' @rdname Methods
aggregate.rfexpr <- function(x, ...) aggregate(deref(x), ...)

#' @export
#' @method coef rfexpr
#' @importFrom stats coef
#' @rdname Methods
coef.rfexpr <- function(object, ...) coef(deref(object), ...)

#' @export
#' @method confint rfexpr
#' @importFrom stats confint
#' @rdname Methods
confint.rfexpr <- function(object, parm, level=0.95, ...) confint(deref(object), parm, level=level, ...)

#' @export
#' @method fitted rfexpr
#' @importFrom stats fitted
#' @rdname Methods
fitted.rfexpr <- function(object, ...) fitted(deref(object), ...)

#' @export
#' @method median rfexpr
#' @importFrom stats median
#' @rdname Methods
median.rfexpr <- function(x, na.rm = FALSE, ...) median(deref(x), na.rm = na.rm, ...)

#' @export
#' @method model.frame rfexpr
#' @importFrom stats model.frame
#' @rdname Methods
model.frame.rfexpr <- function(formula, ...) model.frame(deref(formula), ...)

#' @export
#' @method model.matrix rfexpr
#' @importFrom stats model.matrix
#' @rdname Methods
model.matrix.rfexpr <- function(object, ...) model.matrix(deref(object), ...)

#' @export
#' @method na.omit rfexpr
#' @importFrom stats na.omit
#' @rdname Methods
na.omit.rfexpr <- function(object, ...) na.omit(deref(object), ...)

#' @export
#' @method plot rfexpr
#' @rdname Methods
plot.rfexpr <- function(x, y, ...) plot(deref(x), deref(y), ...)

#' @export
#' @method predict rfexpr
#' @importFrom stats predict
#' @rdname Methods
predict.rfexpr <- function(object, ...) predict(deref(object), ...)

#' @export
#' @method residuals rfexpr
#' @importFrom stats residuals
#' @rdname Methods
residuals.rfexpr <- function(object, ...) residuals(deref(object), ...)

#' @export
#' @method summary rfexpr
#' @rdname Methods
summary.rfexpr <- function(object, ...) summary(deref(object), ...)

#' @export
#' @method terms rfexpr
#' @importFrom stats terms
#' @rdname Methods
terms.rfexpr <- function(x, ...) terms(deref(x), ...)

#' @export
#' @method vcov rfexpr
#' @importFrom stats vcov
#' @rdname Methods
vcov.rfexpr <- function(object, ...) vcov(deref(object), ...)

#' @export
#' @method window rfexpr
#' @importFrom stats window
#' @rdname Methods
window.rfexpr <- function(x, ...) window(deref(x), ...)
