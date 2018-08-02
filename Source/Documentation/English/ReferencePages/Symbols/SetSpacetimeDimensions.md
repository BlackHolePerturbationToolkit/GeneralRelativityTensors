{
  "More Information" -> {
      "SetSpacetimeDimensions will fail without IgnoreWarnings->True, and for good reason. You almost never want to use this function."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "SpacetimeDimensions[gK]",
    "gK3 = SetSpacetimeDimensions[gK,3,IgnoreWarnings->True]",
    "SpacetimeDimensions[gK3]"
    },

    "See Also" ->
    {"SetTensorKeyValue","SpacetimeDimensions"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
