#' Get the import expression for a device
#' 
#' Returns the import expression for all device in LightLogR.
#' 
#' These expressions are used to import and prepare data from specific devices.
#' The list is made explicit, so that a user, requiring slight changes to the
#' import functions, (e.g., because a timestamp is formatted differently) can
#' modify or add to the list. The list can be turned into a fully functional 
#' import function through `import_adjustment()`.
#' 
#' @return A list of import expressions for all supported devices
#' @export
#' @seealso [import_Dataset], [import_Dataset]
#' 
#' @examples ll_import_expr()[1]
#' 
ll_import_expr <- function() {
  import_expr
}