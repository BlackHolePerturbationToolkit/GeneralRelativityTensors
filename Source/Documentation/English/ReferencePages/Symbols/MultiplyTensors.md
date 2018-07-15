{
  "More Information" -> {
      "It is generally simpler to call MergeTensors than to call MultiplyTensors (which MergeTensors does internally)."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "ricSS = RicciScalar[gS]",
    "gRicScaS = MultiplyTensors[ricSS, gS, \"ActWith\" -> Simplify]",
    "gRicScaS // TensorName",
    "gRicScaS // TensorValues"
    },

    "See Also" ->
    {"MergeTensors","ContractIndices","MultiplyTensorScalar","AddTensors"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors"
    }

}
