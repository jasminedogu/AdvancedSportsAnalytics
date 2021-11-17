field_goal_probability <- function(x) {
  exp(-5.832 + 0.0861*(x)) / (1 + exp(-5.832 + 0.0861*(x)))
}
