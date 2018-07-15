{
  "More Information" -> {
      "A Tensor's TensorName is used for caching.",
      "Automatically generated TensorNames end in -Auto.",
      "A TensorName that ends in -Auto is not cached.",
      "If no TensorDisplayName is given when a Tensor is formed, \
the TensorName will be used for the TensorDisplayName also."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "TensorName[gS]",
    "ricS = RicciTensor[gS]",
    "TensorName[gS]",
    "dgS = CovariantD[gS,-\[Gamma]]",
    "TensorName[MergeTensors[dgS]]"
    },

    "See Also" ->
    {"TensorDisplayName"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values"
    }

}
