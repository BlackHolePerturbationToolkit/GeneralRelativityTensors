{
  "More Information" -> {
      "It is generally simpler to call MergeTensors than to individually call the other functions that it calls."
  },

  "Basic Examples" -> {
    "gRN = ToMetric[\"ReissnerNordstrom\"]",
    "ricTRN = RicciTensor[gRN]",
    "ricSRN = RicciScalar[gRN]",
    "einRNExpr = ricTRN[-\[Alpha], -\[Beta]] - gRN[-\[Alpha], -\[Beta]] ricSRN/2",
    "einRN = MergeTensors[einRNExpr, {\"EinsteinRN\", \"G\"}, ActWith -> Simplify]",
    "einRN // TensorValues"
    },

    "See Also" ->
    {"ContractIndices","MultiplyTensors","MultiplyTensorScalar","SumTensors"}

}
