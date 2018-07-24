{
  "More Information" -> {
      "SetDimensions will fail without IgnoreWarnings->True, and for good reason. You almost never want to use this function."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "Dimensions[gK]",
    "gK3 = SetDimensions[gK,3,IgnoreWarnings->True]",
    "Dimensions[gK3]"
    },

    "See Also" ->
    {"SetTensorKeyValue"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
