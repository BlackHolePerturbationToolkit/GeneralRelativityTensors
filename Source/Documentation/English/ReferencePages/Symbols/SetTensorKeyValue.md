{
  "More Information" -> {
      "SetTensorKeyValue should usually be avoided in favor of higher-level functions."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "TensorName[gK]",
    "gKNew = SetTensorKeyValue[gK,\"Name\",\"KerrNew\"]",
    "TensorName[gKNew]"
    },

    "See Also" ->
    {"TensorName","SetTensorName","ActOnTensorValues","SetTensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
