# Calculate clustering index

#' @name cluster-index
#' @title Calculate clustering index
#' @param x numerical matrix of original data (dimension: sample size times
#' number of variate)
#' @param cluster integer vector or matrix of the clustering results. 
#' see 'Details'
#' @param dist distance matrix of \code{x}, used by \code{'silhouette'} and 
#' \code{'c_index'}. If missing, then the value will be automatically 
#' calculated from \code{\link[stats]{dist}} function with additional arguments
#' specified by \code{dist_args}
#' @param dist_args see \code{dist}
#' @param methods cluster index criterion; a full list can be obtained via
#' \code{cluster_index_methods()}. Alternatively, you can specify 
#' \code{methods='default'} or \code{methods='all'} to apply default or all
#' criterion accordingly.
#' @param index the table returned by function \code{cluster_index}
#' @param check see 'Details'
#' @param simplify whether to return unlisted method names
#' @details When \code{cluster} is an integer vector, its length must be equal
#' to the sample size. The observations with the same integer are considered 
#' to be in the same cluster. If \code{cluster} is a matrix, then its row must 
#' be consistent with the sample size.
#' 
#' For \code{'ball_hall'}, \code{'trace_w'}, \code{'trace_wib'}, and 
#' \code{'log_ss_ratio'}, \code{cluster} must be a matrix so 
#' \code{best_criterion} and \code{best_nclusters} can work properly. 
#' In addition, the number of clusters indicated by \code{cluster} columns
#' must be increasing. The \code{cluster_index} will check \code{cluster} by 
#' default, which can be disabled by specifying \code{check=FALSE}
#' 
#' @export
cluster_index_methods <- function(simplify = TRUE){
  re <- list(
    # the followings use max(v)
    max_criteria = c("calinski_harabasz", "dunn", "gamma", "pbm", "point_biserial", "ratkowsky_lance", "silhouette", "tau", "wemmert_gancarski"),
    
    # the followings use min(v)
    min_criteria = c("banfeld_raftery", "c_index", "davies_bouldin", "g_plus", "mcclain_rao", "ray_turi", "sd_scat", "sd_dis"),
    
    # the followings use max(diff(diff(v)))
    max_diff2_criteria = c("ball_hall", "trace_w", "trace_wib"),
    
    # the followings use min(diff(diff(v)))
    min_diff2_criteria = c("log_ss_ratio")
  )
  if(simplify) {
    re <- unlist(re)
  }
  re
}

#' @rdname cluster-index
#' @export
cluster_index <- function(
  x, cluster, dist, dist_args = list(), 
  methods = "default", check = TRUE
) {
  methods <- unique(tolower(methods))
  all_methods <- cluster_index_methods(simplify = FALSE)
  if("all" %in% methods) {
    methods <- unlist(all_methods)
  } else if ("default" %in% methods) {
    methods <- c(all_methods$max_criteria, all_methods$min_criteria)
  } else {
    methods <- methods[methods %in% unlist(all_methods)]
  }
  
  if(!is.matrix(x)) {
    x <- as.matrix(x)
  }
  rownames(x) <- NULL
  
  env <- dipsaus::fastmap2()
  if(missing(dist)) {
    get_dist <- function(){
      if(is.null(env$dist)) {
        dist_args <- as.list(dist_args)
        dist_args$x <- x
        env$dist <- do.call(calculate_distance, dist_args)
        env$dist <- as.matrix(env$dist)
      }
      env$dist
    }
  } else {
    env$dist <- as.matrix(dist)
    get_dist <- function() { env$dist }
  }
  
  # vectorize cluster
  if(!is.matrix(cluster)) {
    cluster <- matrix(cluster, nrow = nrow(x))
  }
  if(nrow(cluster) != nrow(x)) {
    stop("`cluster_index`: `cluster` must be a vector or a matrix with length to be multiple of `nrows(x)`.")
  }
  dm <- dim(cluster)
  cluster <- as.integer(factor(cluster, levels = sort(unique(as.vector(cluster)))))
  dim(cluster) <- dm
  
  abbr_methods <- abbreviate(gsub("[^a-z]", "", methods))
  
  if(check && ncol(cluster)) {
    method_list <- cluster_index_methods(simplify = FALSE)
    diff_methods <- c(method_list$max_diff2_criteria, method_list$min_diff2_criteria)
    abbr_diff_methods <- abbreviate(gsub("[^a-z]", "", diff_methods))
    if(any(abbr_diff_methods %in% abbr_methods)) {
      # need to check clusters, making sure the number is going up by 1
      tmp <- apply(cluster, 2, function(x) {
        length(unique(x))
      })
      if(!all(diff(tmp) >= 0)) {
        stop("``: clustering index with method [", paste(diff_methods[abbr_diff_methods %in% abbr_methods], collapse = ","), "] requires the clustering number to increase.")
      }
    }
  }
  
  # native methods using dist
  native_mathods <- c("slht", "cndx")
  native_mathods <- native_mathods[native_mathods %in% abbr_methods]
  
  re <- data.frame(strategy = seq_len(ncol(cluster)))
  
  # ------------------------ Silhouette -------------------------
  if("slht" %in% native_mathods) {
    get_dist()
    re$silhouette <- apply(cluster, 2, function(idx) {
      print(idx)
      if(length(unique(idx)) == 1) { return(NA) }
      silhouette_widths <- cluster::silhouette(idx, env$dist)
      if(is.matrix(silhouette_widths) && ncol(silhouette_widths) == 3) {
        return(mean(cluster::silhouette(idx, env$dist)[, 3]))
      }
      return(NA)
    })
  }
  
  # ------------------------ C_index -------------------------
  if("cndx" %in% native_mathods) {
    get_dist()
    env$dist_diag <- diag(env$dist)
    env$dist_sorted <- sort(env$dist[upper.tri(env$dist)])
    re$c_index <- apply(cluster, 2, function(idx) {
      
      tmp <- sapply(unique(idx), function(k) {
        sel <- idx == k
        
        s_w_k <- (sum(env$dist[sel, sel]) - sum(env$dist_diag[sel])) / 2
        nk <- sum(sel)
        c(s_w_k, (nk*(nk-1)) / 2)
        
      })
      s_w <- sum(tmp[1, ])
      n_w <- sum(tmp[2, ])
      s_min <- sum(env$dist_sorted[seq_len(n_w)])
      s_max <- sum(env$dist_sorted[(length(env$dist_sorted) + 1) - seq_len(n_w)])
      
      ## The result should be identical tothe following if 
      ## dist() uses default settings
      # clusterCrit::intCriteria(x, idx, "C_index")
      if(abs(s_max - s_min) < 1e-7) {
        return(1)
      }
      re <- (s_w - s_min) / (s_max - s_min)
      if(abs(re) < 1e-7) {
        return(1)
      }
      re
      
    })
  }
  
  # -------------------------- other methods ----------------------------
  sel <- !abbr_methods %in% native_mathods
  extra_methods <- methods[sel]
  
  extra <- do.call("rbind", apply(cluster, 2, function(idx) {
    as.data.frame(clusterCrit::intCriteria(
      x, idx, extra_methods
    ))
  }))
  
  re <- cbind(re, extra)
  
  for(nm in methods) {
    v <- re[[nm]]
    if(nm %in% c("davies_bouldin", "g_plus", "ray_turi", "xie_beni", "trace_wib")) {
      invalids <- !is.finite(v) | v == 0
    } else if(nm %in% "wemmert_gancarski") {
      invalids <- !is.finite(v) | v == 1
    } else {
      invalids <- !is.finite(v) | v > 1e300 | v < -1e300
    }
    v[invalids] <- NA
    re[[nm]] <- v
  }
  
  re <- re[, methods]
  attr(re, "clusters") <- cluster
  class(re) <- c("cluster_index", class(re))
  re
}

#' @rdname cluster-index
#' @export
best_criterion <- function(index) {
  stopifnot(inherits(index, "cluster_index"))
  re <- index
  nms <- names(re)
  
  criteria_list <- cluster_index_methods(simplify = FALSE)
  cnames <- criteria_list$max_criteria[criteria_list$max_criteria %in% nms]
  rmax <- structure(
    sapply(cnames, function(nm) {
      v <- re[[nm]]
      sel <- which(!is.na(v))
      if(!length(sel)) { return(NA) }
      sel[which.max(v[sel])]
    }),
    names = cnames
  )
  
  cnames <- criteria_list$min_criteria[criteria_list$min_criteria %in% nms]
  rmin <- structure(
    sapply(cnames, function(nm) {
      v <- re[[nm]]
      sel <- which(!is.na(v))
      if(!length(sel)) { return(NA) }
      sel[which.min(v[sel])]
    }),
    names = cnames
  )
  
  cnames <- criteria_list$max_diff2_criteria[criteria_list$max_diff2_criteria %in% nms]
  rmax_diff2 <- structure(
    sapply(cnames, function(nm) {
      v <- diff(diff(re[[nm]]))
      sel <- which(!is.na(v))
      if(!length(sel)) { return(NA) }
      sel[which.max(v[sel])] + 1
    }),
    names = cnames
  )
  
  cnames <- criteria_list$min_diff2_criteria[criteria_list$min_diff2_criteria %in% nms]
  rmin_diff2 <- structure(
    sapply(cnames, function(nm) {
      v <- diff(diff(re[[nm]]))
      sel <- which(!is.na(v))
      if(!length(sel)) { return(NA) }
      sel[which.min(v[sel])] + 1
    }),
    names = cnames
  )
  r <- c(rmin, rmax, rmax_diff2, rmin_diff2)
  r[nms]
}

#' @rdname cluster-index
#' @export
best_nclusters <- function(index) {
  re <- best_criterion(index)
  clusters <- attr(index, "clusters")
  
  structure(
    sapply(re, function(r) {
      if(is.finite(r) && r <= ncol(clusters)) {
        structure(
          length(unique(clusters[, r])),
          index = r,
          cluster = clusters[, r]
        )
      } else { NA }
    }),
    names = names(re)
  )
}


# x <- results$indata_analysis
# cluster <- results$cluster_table$Cluster
# 
# cvi <- cluster_index(x, cluster, dist = )
# best_nclusters(cvi)


