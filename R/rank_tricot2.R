#' Build Plackett-Luce rankings from tricot ranking dataset 
#'
#' Create an object of class "rankings" from tricot ranking data. 
#'
#' @author Kauê de Sousa
#' @family rank functions
#' @param id an index of \code{data} indicating the id column
#' @inheritParams rank_tricot
#' @param strict logical. If \code{TRUE}, reject missing or duplicate
#'   id/item/rank records and require positive integer ranks with no ties
#'   within an id. Defaults to \code{FALSE} to preserve the legacy
#'   warning-and-skip behaviour
#' @param ... additional arguments passed to methods. See details
#' @return a PlackettLuce "rankings" object, which is a matrix of dense rankings 
#' @seealso \code{\link[PlackettLuce]{rankings}}, \code{\link{nicabean}}
#' @references 
#' 
#' van Etten J., et al. (2016). Experimental Agriculture, 55(S1), 275–296.
#' \doi{https://doi.org/10.1017/S0014479716000739}
#' 
#' @examples
#' 
#' library("gosset")
#' 
#' R = nicabean$trial
#' 
#' R = R[R$trait == "Vigor", ]
#' 
#' names(R)
#' 
#' R = rank_tricot2(R, id = "id", items = "item", input = "rank")
#' 
#' @importFrom PlackettLuce as.rankings group
#' @export
rank_tricot2 = function(data,
                        id,
                        items, 
                        input, 
                        strict = FALSE,
                        ...) {
  
  if (!is.data.frame(data)) stop('data must be a data.frame')
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop('strict must be TRUE or FALSE')
  }
  resolve_column = function(col) {
    if (length(col) != 1L || is.na(col)) stop('Column selectors must be scalar and non-missing')
    if (is.numeric(col)) {
      if (col < 1 || col > ncol(data) || col != floor(col)) stop('Column index out of range')
      return(names(data)[col])
    }
    if (!is.character(col) || !(col %in% names(data))) stop('Unknown column name')
    col
  }
  id = resolve_column(id)
  items = resolve_column(items)
  input = resolve_column(input)
  if (nrow(data) == 0L) stop('data must contain at least one row')

  # columns
  x = data
  id_col     = id
  item_col   = items
  value_col  = input
  
  # order-preserving unique (same as your code)
  ids        = x[[id_col]][!duplicated(x[[id_col]])]
  items      = x[[item_col]][!duplicated(x[[item_col]])]
  
  # map each row to matrix coordinates
  i = match(x[[id_col]],   ids)
  j = match(x[[item_col]], items)
  
  # basic sanity checks (optional but helpful)
  bad_rows = which(is.na(i) | is.na(j) | is.na(x[[value_col]]))
  if (length(bad_rows)) {
    msg = sprintf("Found %d rows with NA in id/item/value.", length(bad_rows))
    if (strict) stop(msg)
    warning(paste(msg, "Skipping these rows."))
  }
  
  # detect duplicates of the same (id, item)
  dupes = duplicated(cbind(i, j)) | duplicated(cbind(i, j), fromLast = TRUE)
  if (any(dupes, na.rm = TRUE)) {
    d = x[dupes, c(id_col, item_col, value_col)]
    msg = sprintf("Found %d duplicate (id, item) pairs.", nrow(d))
    if (strict) stop(msg)
    warning(paste(msg, "Last value wins."))
  }
  
  if (strict) {
    values = suppressWarnings(as.numeric(as.character(x[[value_col]])))
    valid = is.finite(values) & values > 0 & values == floor(values)
    if (!all(valid)) stop('Ranks must be finite positive integers in strict mode')
    groups = split(values, x[[id_col]])
    if (any(vapply(groups, function(z) anyDuplicated(z) > 0L, logical(1)))) {
      stop('Tied ranks within an id are not permitted in strict mode')
    }
    if (any(vapply(groups, function(z) !setequal(z, seq_along(z)), logical(1)))) {
      stop('Each id must have consecutive ranks from 1 to its number of items')
    }
  }

  # build matrix (zeros = unranked)
  m = matrix(0,
             nrow = length(ids),
             ncol = length(items),
             dimnames = list(ids, items))
  
  # write values; last occurrence wins automatically
  idx = which(!is.na(i) & !is.na(j) & !is.na(x[[value_col]]))
  m[cbind(i[idx], j[idx])] = as.numeric(as.character(x[[value_col]][idx]))
  
  # convert to rankings
  r = PlackettLuce::as.rankings(m)
  
  return(r)
  
}
