{
  "More Information" -> {
   "RicciScalar returns a Tensor with no indices."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "ricSS = RicciScalar[gS, \"ActWith\" -> Simplify]",
    "TensorValues[ricSS]"
    },

    "See Also" ->
    {"ChristoffelSymbol","RiemannTensor","RicciTensor","EinsteinTensor","WeylTensor","KretschmannScalar"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Built in common Tensors"
    }

}
