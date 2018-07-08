{
  "More Information" -> {
   "RicciScalar returns a Tensor with no indices."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "ricSS = RicciScalar[gS, SimplifyFunction -> Simplify]",
    "TensorValues[ricSS]"
    },

    "See Also" ->
    {"ChristoffelSymbol","RiemannTensor","RicciTensor","EinsteinTensor"}

}
