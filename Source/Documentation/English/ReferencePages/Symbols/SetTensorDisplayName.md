{
  "More Information" -> {
      "TensorName is used for caching, so if it is changed without care, strange results are possible."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "TensorName[gK]",
    "gKNew = SetTensorDisplayName[gK,\"k\"]",
    "TensorDisplayName[gKNew]"
    },

    "See Also" ->
    {"TensorDisplayName","SetTensorKeyValue","SetTensorName","ActOnTensorValues","SetTensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
