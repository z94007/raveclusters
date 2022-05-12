progress <- function (title, max = 1, session = getDefaultReactiveDomain(), 
          quiet = FALSE, ...) {
  return(dipsaus::progress2(title = title, max = max, session = session, 
                            quiet = quiet, ..., log = raveio::catgl))
}
