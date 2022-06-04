#' @export
calculate_distance <- function(x, method = c(
  "euclidean",
  "maximum",
  "manhattan",
  "canberra",
  "binary",
  "minkowski",
  "DTW",
  "1 - correlation",
  "euclidean (weighted)",
  "maximum (weighted)",
  "manhattan (weighted)",
  "canberra (weighted)",
  "minkowski (weighted)",
  "DTW (weighted)",
  "1 - correlation (weighted)"
), ...) {
  method <- match.arg(method)
  
  if(grepl(pattern = " \\(weighted\\)", x = method)) {
    sd <- apply(x, 2, function(x) {
      qt <- quantile(x, c(0.025, 0.975))
      sd(x[x >= qt[1] & x <= qt[2]])
    })
    sd <- sd / sum(sd)
    x <- t(t(as.matrix(x)) * sd)
  }
  method <- gsub(pattern = " \\(weighted\\)", replacement = "", x = method)
  
  if(isTRUE(method == '1 - correlation')){
    dis = as.dist(1-cor(t(x)))
  } else if(isTRUE(method == 'DTW')){
    dis = as.dist(dtw::dtwDist(x))
  } else {
    dis = dist(x, method = method, ...)
  }
  dis
}