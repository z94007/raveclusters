# Functions ported from ravebuiltins

remove_hemisphere_labels <- function(levels) {
  
  # if(dipsaus::package_installed("ravebuiltins")) {
  #   ravebuiltins::remove_hemisphere_labels(levels)
  # } else {
    stringr::str_replace_all(levels, c(`L ` = "", `R ` = ""))
  # }
  
}

remove_gyrus_sulcus_labels <- function(levels) {
  # if(dipsaus::package_installed("ravebuiltins")) {
  #   ravebuiltins::remove_gyrus_sulcus_labels(levels)
  # } else {
    stringr::str_replace_all(levels, c(`GS ` = "", `G ` = "", `S ` = ""))
  # }
}


get_palette <- function (pname, get_palettes = FALSE, get_palette_names = FALSE) {
  
  .palettes <- list(
    `Beautiful Field` = c("orange", "dodgerblue3", "darkgreen", "orangered", "brown", "purple3"), 
    Perm4_0 = c("#2297E6", "#F5C710", "#61D04F", "#DF536B", "#CD0BBC"), 
    `Black+tan` = c("#1A1A19", "#A25D34", "#353634", "#D0A380"), 
    Accent = c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666"),
    Dark2 = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666"),
    Paired = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"), 
    Pastel1 = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec"),
    Pastel2 = c("#b3e2cd", "#fdcdac", "#cbd5e8", "#f4cae4", "#e6f5c9", "#fff2ae", "#f1e2cc", "#cccccc"), 
    Set1 = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf"),
    Set2 = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"), 
    Set3 = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5"),
    BlueYellow = grDevices::hcl.colors(101, palette = "BluYl", rev = TRUE)
  )
  if (missing(pname)) {
    if (get_palette_names) 
      return(names(.palettes))
    return(.palettes)
  }
  pal <- .palettes[[pname]]
  if (is.null(pal)) {
    warning("Invalid palette requested: ", pname, ". Returning random palette")
    pal <- .palettes[[sample(seq_along(.palettes), 1)]]
  }
  return(pal)
}


get_heatmap_palette <- function (pname,
          get_palettes = FALSE,
          get_palette_names = FALSE) {
  .heatmap_palettes <- list(
    BlueWhiteRed = c(
      "#67001f",
      "#b2182b",
      "#d6604d",
      "#f4a582",
      "#fddbc7",
      "#ffffff",
      "#d1e5f0",
      "#92c5de",
      "#4393c3",
      "#2166ac",
      "#053061"
    ) %>% rev,
    BlueGrayRed = rev(
      c(
        "#67001f",
        "#b2182b",
        "#d6604d",
        "#f4a582",
        "#b4b4b4",
        "#92c5de",
        "#4393c3",
        "#2166ac",
        "#053061"
      )
    ),
    Spectral = c(
      "#9e0142",
      "#d53e4f",
      "#f46d43",
      "#fdae61",
      "#fee08b",
      "#ffffbf",
      "#e6f598",
      "#abdda4",
      "#66c2a5",
      "#3288bd",
      "#5e4fa2"
    ) %>% rev,
    BrownWhiteGreen = c(
      "#543005",
      "#8c510a",
      "#bf812d",
      "#dfc27d",
      "#f6e8c3",
      "#f5f5f5",
      "#c7eae5",
      "#80cdc1",
      "#35978f",
      "#01665e",
      "#003c30"
    ),
    PinkWhiteGreen = c(
      "#8e0152",
      "#c51b7d",
      "#de77ae",
      "#f1b6da",
      "#fde0ef",
      "#f7f7f7",
      "#e6f5d0",
      "#b8e186",
      "#7fbc41",
      "#4d9221",
      "#276419"
    ),
    PurpleWhiteGreen = c(
      "#40004b",
      "#762a83",
      "#9970ab",
      "#c2a5cf",
      "#e7d4e8",
      "#f7f7f7",
      "#d9f0d3",
      "#a6dba0",
      "#5aae61",
      "#1b7837",
      "#00441b"
    ),
    OrangeWhitePurple = c(
      "#7f3b08",
      "#b35806",
      "#e08214",
      "#fdb863",
      "#fee0b6",
      "#f7f7f7",
      "#d8daeb",
      "#b2abd2",
      "#8073ac",
      "#542788",
      "#2d004b"
    ),
    BlackWhiteRed = c(
      "#67001f",
      "#b2182b",
      "#d6604d",
      "#f4a582",
      "#fddbc7",
      "#ffffff",
      "#e0e0e0",
      "#bababa",
      "#878787",
      "#4d4d4d",
      "#1a1a1a"
    ) %>%
      rev,
    BlueYellowRed = c(
      "#a50026",
      "#d73027",
      "#f46d43",
      "#fdae61",
      "#fee090",
      "#ffffbf",
      "#e0f3f8",
      "#abd9e9",
      "#74add1",
      "#4575b4",
      "#313695"
    ) %>% rev,
    GreenYellowRed = c(
      "#a50026",
      "#d73027",
      "#f46d43",
      "#fdae61",
      "#fee08b",
      "#ffffbf",
      "#d9ef8b",
      "#a6d96a",
      "#66bd63",
      "#1a9850",
      "#006837"
    ) %>%
      rev
  )
  if(!isTRUE(pname %in% names(.heatmap_palettes))) {
    stop("`get_heatmap_palette`: Invalid palette requested: ", pname)
  }
  pal <- .heatmap_palettes[[pname]]
  attr(pal, "name") = pname
  return(pal)
}

get_cex_for_multifigure <- function () 
{
  cex_multiplier <- 1
  if (shiny::isRunning()) {
    if (any(par("mfrow") > 2)) {
      cex_multiplier = 1/0.66
    }
    else if (all(par("mfrow") == 2)) {
      cex_multiplier <- 1/0.88
    }
  }
  return(cex_multiplier)
}

plotting_to_file <- function () {
  i = get0(".Devices", envir = baseenv(), ifnotfound = list("-1"), 
           inherits = FALSE)
  if ("-1" == i[[1]]) {
    return(FALSE)
  }
  if (length(i) < dev.cur()) {
    return(FALSE)
  }
  i = i[[dev.cur()]]
  return(isTRUE("dipsaus_dev_name" %in% names(attributes(i)) || 
                  i %in% c("pdf", "png", "jpeg")))
}

rave_title <- function (main, cex = 1, col, font = 1) {
  if(missing(col) || is.null(col)) {
    col <- par("fg")
  }
  if (plotting_to_file()) {
    cex <- 1
  } else {
    cex <- shiny_cex.main * cex
  }
  title(main = list(main, cex = cex * get_cex_for_multifigure(), 
                    col = col, font = font))
}