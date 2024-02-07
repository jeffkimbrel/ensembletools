#' Ensemble data class
#'
#' @export

ensemble <- S7::new_class(
  "ensemble",
  properties = list(
    path = S7::class_character,
    solutions = S7::class_data.frame,
    nmds = S7::class_list
  ),
  constructor = function(path) {

    solutions = parse_solutions(path,
                                scale = F)


    S7::new_object(S7::S7_object(),
      path = path,
      solutions = solutions,
      nmds = list()
    )
  }
)
