#' Build Plackett-Luce rankings from tricot ranking dataset 
#'
#' Create an object of class "rankings" from tricot ranking data. 
#'
#' @author Kauê de Sousa
#' @family rank functions
#' @param id an index of \code{data} indicating the id column
#' @inheritParams rank_tricot
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
                        ...) {
  
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
    warning(sprintf("Skipping %d rows with NA in id/item/value.", length(bad_rows)))
  }
  
  # detect duplicates of the same (id, item)
  dupes = duplicated(cbind(i, j)) | duplicated(cbind(i, j), fromLast = TRUE)
  if (any(dupes, na.rm = TRUE)) {
    d = x[dupes, c(id_col, item_col, value_col)]
    warning(sprintf("Found %d duplicate (id, item) pairs. Last value wins.", nrow(d)))
  }
  
  # build matrix (zeros = unranked)
  m = matrix(0,
             nrow = length(ids),
             ncol = length(items),
             dimnames = list(ids, items))
  
  # write values; last occurrence wins automatically
  idx = which(!is.na(i) & !is.na(j) & !is.na(x[[value_col]]))
  m[cbind(i[idx], j[idx])] = as.numeric(x[[value_col]][idx])
  
  # convert to rankings
  r = PlackettLuce::as.rankings(m)
  
  return(r)
  
}
