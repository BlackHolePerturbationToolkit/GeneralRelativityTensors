{
  "More Information" -> {
      "Like all functions in GeneralRelativityTensors, ActOnTensorValues does not \
change the values of the Tensor it is called on."
  },

  "Basic Examples" -> {
    "ric = RicciTensor[ToMetric[\"Schw\"]]",
    "TensorValues[ric]",
    "ricSimp = ActOnTensorValues[Simplify, ric]",
    TensorValues[ricSimp]
    },

    "See Also" ->
    {"ShiftIndices","MergeTensors"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors"
    }

}
