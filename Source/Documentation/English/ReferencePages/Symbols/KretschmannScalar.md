{
  "More Information" -> {
   "KretschmannScalar returns a Tensor with no indices."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "kreSS = KretschmannScalar[gS, \"ActWith\" -> Simplify]",
    "TensorValues[kreSS]"
    },

    "See Also" ->
    {"ChristoffelSymbol","RiemannTensor","RicciTensor","EinsteinTensor","WeylTensor","RicciScalar"}

}
