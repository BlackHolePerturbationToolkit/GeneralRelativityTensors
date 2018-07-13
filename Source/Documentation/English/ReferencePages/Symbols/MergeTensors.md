{
  "More Information" -> {
      "It is generally simpler to call MergeTensors than to individually call the other functions that it calls.",
      "For complicated expressions, MergeTensors may initially fail to completely merge the expression into one Tensor. \
 By default MergeTensors will call itself again and continue to combine terms. The number of times it may call itself \
 is controlled by the Option NestQuantity."
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
