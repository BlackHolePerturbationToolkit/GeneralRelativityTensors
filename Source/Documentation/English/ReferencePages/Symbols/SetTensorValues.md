{
  "More Information" -> {
      "SetTensorValues clears any cached values associated with the TenorName of t."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "TensorValues[gK]",
    "gKNull = SetTensorValues[gK,Table[0,{4},{4}]]",
    "TensorValues[gKNull]"
    },

    "See Also" ->
    {"SetTensorKeyValue","ActOnTensorValues","TensorValues","RawTensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
