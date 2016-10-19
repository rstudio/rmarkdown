worldPhones <- function(id, selected = "Europe", height = 600) {
  # emit the module's server code into a context = "server" chunk
  rmarkdown::shiny_prerendered_chunk(context = "server", code = paste0(
    'callModule(worldPhonesServer, "', id, '")'
  ))

  # return module UI
  worldPhonesUI(id, selected, height)
}