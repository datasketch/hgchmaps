
click_functions <- function(viz, frtype = NULL, id_click = NULL) {
  if (is.null(id_click)) return()
  click_func <- JS(paste0("function(event) {Shiny.onInputChange('", id_click, "', {id:event.point.name, timestamp: new Date().getTime()});}"))
  click_func
}
