{
  "More Information" -> {
      "ValidTensorExpressionQ only returns True/False. It does not explain what is wrong with the expression.",
      "ValidateTensorExpression is like ValidTensorExpressionQ, but will give error messages and Abort."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "ricS = RicciTensor[gS]",
    "ValidTensorExpressionQ[gS[-\[Alpha],-\[Beta]]+ricS[-\[Alpha],-\[Beta]]]",
    "ValidTensorExpressionQ[gS[-\[Alpha],-\[Beta]]ricS[-\[Alpha],-\[Beta]]]",
    "gRN = ToMetric[\"ReissnerNordstrom\"]",
    "ValidTensorExpressionQ[gS[-\[Alpha],-\[Beta]]+gRN[-\[Alpha],-\[Beta]]]"
    },

    "See Also" ->
    {"ValidateTensorExpression"}

}
