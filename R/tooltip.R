
#' @export
hgch_tooltip <- function(nms, label_ftype = NULL, tooltip) {
  if (is.null(nms)) stop("Enter names")
  nms <- nms
  nms <- gsub("[][!()*`|]", "",nms)
  nms <- gsub("[\r\n]", " ", nms)
  label_ftype_clean <- gsub("[][!()*`|{}]", "", label_ftype)
  label_ftype_clean <- gsub("[\r\n]", " ", label_ftype_clean)
  nms_names <- names(nms)

  if (tooltip == "") tooltip <- NULL
  if (is.null(tooltip)) {
    tooltip  <- paste0(purrr::map(seq_along(label_ftype), function(i) {
      paste0(label_ftype[i], ": {", label_ftype_clean[i], "}")
    }) %>% unlist(), collapse = "<br/>")
  } else {
    tooltip <- gsub("[][()*`|]", "", tooltip)#gsub("[][!#$()*,.:;<=>@^`|~.", "", tooltip)
  }

  points <- gsub("\\{|\\}", "",
                 stringr::str_extract_all(tooltip, "\\{.*?\\}")[[1]])

  if(!all(points %in% label_ftype_clean)) stop("all variables within braces must be contained in the loaded data")

  if (identical(points, character())) {
    tooltip <- tooltip
  } else {
    l <- purrr::map(seq_along(points), function(i){

      true_points <-  paste0("{",names(nms[match(points[i], nms)]),"_label}")
      tooltip <<- gsub(paste0("\\{",points[i], "\\}"), true_points, tooltip)
    })[[length(points)]]
    }

  tooltip
}

