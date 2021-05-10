#' @export
data_prep <- function (data, ftype, agg, ptage_col, ...) {


  f <- homodatum::fringe(data)
  nms <- homodatum::fringe_labels(f)
  nms[length(nms)+1] <- c("%")
  names(nms) <- c(names(nms)[-length(nms)], "..percentage")
  nms[length(nms)+1] <- c("Count")
  names(nms) <- c(names(nms)[-length(nms)], "..count")
  d <- homodatum::fringe_d(f)
  if (grepl("Gln|Glt", ftype)) {
    d <- d %>% drop_na(a, b)
  } else {
    d <- d %>% drop_na(a)
  }
  frtype <- f$frtype
  dic <- f$dic
  dic$id <- names(d)
  ncols_d <- ncol(d)

  ftype_vec <- stringr::str_split(ftype,pattern = "-") %>% unlist()
  ftype_length <- length(ftype_vec)

  add_cols <- ncols_d != ftype_length




  dd <- d[,1:ftype_length]
  dic_p <- dic %>% dplyr::filter(id %in% names(dd))


  # type data to work
  has_num <- grepl("Num", ftype)
  var_num <- NULL
  agg_var <- "..count"
  if (has_num) {
    var_num <- dic_p %>% filter(hdType %in% "Num") %>% .$id
    agg_var <- names(nms)[grep("Num", ftype_vec)]
  }

  has_cat <- grepl("Cat", ftype)
  var_cat <- NULL
  if (has_cat) var_cat <- dic_p %>% filter(hdType %in% "Cat") %>% .$id

  has_geo <- grepl("Gcd|Gnm", ftype)
  var_group <- NULL
  if (has_geo) var_group <- dic_p %>% filter(hdType %in% c("Gcd", "Gnm")) %>% .$id
  if (!is.null(var_cat)) var_group <- c(var_group, var_cat)

  has_cor <- grepl("Gln|Glt", ftype)
  var_cor <- NULL
  if (has_cor) {
    var_cor <- dic_p %>% filter(hdType %in% c("Gln", "Glt")) %>% .$id
    var_group <- c(var_group, var_cor)
  }

  if (has_geo | has_cor) {
    if (length(var_group) == 1) {
      dd <- function_agg(dd, agg, to_agg = var_num, a)
      ptage_col <- NULL
    } else if (length(var_group) == 2) {
      dd <- function_agg(dd, agg, to_agg = var_num, a, b)
    } else if (length(var_group) == 3) {
      dd <- function_agg(dd, agg, to_agg = var_num, a, b, c)
    }
  }

  if (!is.null(ptage_col))  ptage_col <- names(nms[match(ptage_col, nms)])

  dd <- percentage_data(dd, agg_var = agg_var, by_col = ptage_col)

  if (add_cols) {
    join_cols <- dic_p$id[1:length(var_group)]
    extra_cols <- c(join_cols, setdiff(dic$id, dic_p$id))
    dj <- d[extra_cols]

    if (length(join_cols) == 1) {
      dj <- collapse_data(dj, a)
    } else if (length(join_cols) == 2) {
      dj <- collapse_data(dj, a, b)
    } else if (length(join_cols) == 3) {
      dj <- collapse_data(dj, a, b, c)
    }

    dd <- dd %>% left_join(dj, by = join_cols)

  }




  l <- list(
    data = dd#,
    # dic,
    # nms,
    # nms_tooltip
  )





}

