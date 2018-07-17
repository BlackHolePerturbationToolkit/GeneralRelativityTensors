{
  "More Information" -> {
      "DownValues of Tensors call TensorPattern if the values are Patterns.",
      "It is usually easier to use DownValues than to call TensorPattern directly.",
      "Beware that a simple Pattern like a_ will match both upper and lower indices. \
Make sure to specify explicitly with a_Symbol or -a_Symbol if you don't want to match both.",
"In addition to changing its indices to Patterns, TensorPattern replaces a Tensor's \
values, metric, and curve with the pattern Blank[]."
  },

  "Basic Examples" -> {
    "gRN = ToMetric[\"ReissnerNordstrom\"]",
    "chrRN = ChristoffelSymbol[gRN]",
    "chrRN /. TensorPattern[chrRN,{a_, b_, c_}]:>{a,b,c}",
    "derivFRN = CovariantD[FieldStrengthTensor[\"RN\"], -\[Gamma]]",
    "derivFRN /. chrRN[a_Symbol, -b_Symbol, -c_Symbol] :>  1/2 gRN[a, \[Mu]] (D[gRN[-\[Mu], -b], -c] + D[gRN[-\[Mu], -c], -b] - D[gRN[-b, -c], -\[Mu]])"
    },

    "See Also" ->
    {"Component","ShiftIndices"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors",
      "Pattern matching with Tensors"
    }

}
