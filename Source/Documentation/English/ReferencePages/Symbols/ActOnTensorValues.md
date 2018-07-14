{
  "More Information" -> {
      "Like all functions in the Tensors package, ActOnTensorValues does not \
change the values of the Tensor it is called on."
  },

  "Basic Examples" -> {
    "ric = RicciTensor[ToMetric[\"Schw\"]]",
    "TensorValues[ric]",
    "rieSimp = ActOnTensorValues[Simplify, rie]",
    TensorValues[rieSimp]
    },

    "See Also" ->
    {"ShiftIndices","MergeTensors"}

}
