new_rational <- function(n = integer(), d = integer()) {
  vctrs::vec_assert(n, ptype = integer())
  vctrs::vec_assert(d, ptype = integer())

  vctrs::new_rcrd(list(n = n, d = d), class = "vctrs_rational")
}

rational <- function(n = integer(), d = integer()) {
  c(n, d) %<-% vctrs::vec_cast_common(n, d, .to = integer())
  c(n, d) %<-% vctrs::vec_recycle_common(n, d)

  new_rational(n, d)
}

#x <- rational(1, 1:10)

#' @export format.vctrs_rational
#' @export
format.vctrs_rational <- function(x, ...) {
  n <- vctrs::field(x, "n")
  d <- vctrs::field(x, "d")

  out <- paste0(n, "/", d)
  out[is.na(n) | is.na(d)] <- NA

  out
}

vec_ptype_abbr.vctrs_rational <- function(x, ...) "rtnl"
vec_ptype_full.vctrs_rational <- function(x, ...) "rational"


vec_ptype2.vctrs_rational.vctrs_rational <- function(x, y, ...) new_rational()
vec_ptype2.vctrs_rational.integer <- function(x, y, ...) new_rational()
vec_ptype2.integer.vctrs_rational <- function(x, y, ...) new_rational()

vec_cast.vctrs_rational.vctrs_rational <- function(x, to, ...) x
vec_cast.double.vctrs_rational <- function(x, to, ...) field(x, "n") / field(x, "d")
vec_cast.vctrs_rational.integer <- function(x, to, ...) rational(x, 1)

#vec_c(rational(1, 2), 1L, NA)
#> <rational[3]>
#> [1] 1/2  1/1  <NA>


#x
#> <rational[10]>
#>  [1] 1/1  1/2  1/3  1/4  1/5  1/6  1/7  1/8  1/9  1/10
