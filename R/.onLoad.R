.onLoad <- function(libname, pkgname) {
  # Check if 'vars' is available before attempting to override
  if ("vars" %in% loadedNamespaces()) {
    assignInNamespace("Psi.varest", Psi.varest, ns = "vars")
  } else {
    warning("'vars' package is not loaded; Psi.varest override not applied.")
  }
}
