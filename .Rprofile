# .Rprofile - cargado automÃ¡ticamente por R y por el R Language Server.
# Hace que las constantes/funciones de _config.R sean visibles para todos
# los scripts y para la diagnÃ³stica estÃ¡tica (codetools::checkUsage),
# silenciando los falsos positivos "no visible binding".

local({
  cfg <- file.path(getwd(), "_config.R")
  if (file.exists(cfg)) {
    try(source(cfg, local = FALSE), silent = TRUE)
  }
})
