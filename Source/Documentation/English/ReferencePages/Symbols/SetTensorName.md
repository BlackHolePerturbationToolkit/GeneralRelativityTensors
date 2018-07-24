{
  "More Information" -> {
      "TensorName is used for caching, so if it is changed without care, strange results are possible."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "TensorName[gK]",
    "gKNew = SetTensorName[gK,\"KerrNew\"]",
    "TensorName[gKNew]"
    },

    "See Also" ->
    {"TensorName","SetTensorKeyValue","SetTensorDisplayName","ActOnTensorValues","SetTensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
