

#' @export
cluster_tree <- function(results) {
  
  # indata_analysis <- results$indata_analysis
  cluster_index <- results$cluster_index
  imgbase64 <- results$imgbase64
  if(!inherits(imgbase64, 'fastqueue2')) {
    imgbase64 <- dipsaus::fastqueue2()
  }
  clusters <- attr(cluster_index, "clusters")
  rownames(clusters) <- NULL
  max_nclusters <- ncol(clusters)
  nobs <- nrow(clusters)
  
  # prepare for nodes
  nodes <- lapply(seq_len(max_nclusters), function(layer) {
    list(
      cluster = seq_len(layer),
      layer = layer
    )
  })
  
  edges <- lapply(seq_len(max_nclusters - 1), function(layer) {
    # from layer -> layer+1
    list(
      layer = layer,
      transition = matrix(NA_integer_, nrow = layer, ncol = layer + 1)
    )
  })
  
  for(layer in seq_len(max_nclusters - 1)) {
    sub <- clusters[, c(layer, layer + 1)]
    tmp <- split(seq_len(nobs), sprintf("%s %s", sub[,1], sub[,2]))
    for(idx in tmp) {
      v <- length(idx)
      idx <- sub[idx[[1]], ]
      edges[[layer]]$transition[idx[1], idx[2]] <- v
    }
  }
  
  # convert nodes to node_df
  nodes2 <- lapply(nodes, function(item){
    # item <- nodes[[1]]
    p <- item$layer * (item$layer - 1) / 2
    idx <- item$cluster + p
    image <- imgbase64[idx]
    missing_image <- vapply(image, is.null, FALSE)
    image[missing_image] <- ""
    shape <- rep("image", item$layer)
    shape[missing_image] <- "ellipse"
    data.frame(
      id = item$cluster + p,
      level = item$layer,
      x = seq_len(item$layer),
      # # label = as.character(node_pos),
      shape = shape,
      image = unlist(image)
    )
  })
  node_df <- do.call("rbind", nodes2)
  
  edge_df <-
    do.call("rbind", lapply(seq_len(max_nclusters - 1), function(layer) {
      item <- edges[[layer]]
      from_df <- nodes2[[layer]]
      to_df <- nodes2[[layer + 1]]
      idx1 <- which(!is.na(item$transition), arr.ind = FALSE)
      idx <- arrayInd(idx1, dim(item$transition))
      data.frame(from = from_df$id[idx[, 1]],
                 to = to_df$id[idx[, 2]],
                 label = item$transition[idx1])
    }))
  
  wg <- visNetwork::visNetwork(node_df, edge_df, 
                               width = "100%", 
                               height = "100vh")
  
  # add direction
  wg <- visNetwork::visEdges(wg, arrows = "to")
  wg <- visNetwork::visNodes(
    wg, size = 64,
    shapeProperties = list(useBorderWithImage = FALSE)
  )
  wg <- visNetwork::visHierarchicalLayout(
    wg, direction = "LR", nodeSpacing = 100, treeSpacing = 400,
    levelSeparation = 200, sortMethod = "directed",
    blockShifting = TRUE, edgeMinimization = TRUE,
    parentCentralization = TRUE
  )
  
  # Add interaction properties
  wg <- visNetwork::visInteraction(
    wg, dragNodes = FALSE, zoomSpeed = 0.1
  )
  wg <- visNetwork::visConfigure(wg, enabled = FALSE)
  wg
  
}