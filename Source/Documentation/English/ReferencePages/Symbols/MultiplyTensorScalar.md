{
  "More Information" -> {
      "It is generally simpler to call MergeTensors than to call MultiplyTensorScalar (which MergeTensors does internally)."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "MultiplyTensorScalar[2, gS]",
    "MultiplyTensorScalar[gS, 2]",
    "twoGS = MultiplyTensorScalar[2, gS, {\"2SchwarzschildMetric\", \"2g\"}]",
    "twoGS // TensorValues"
    },

    "See Also" ->
    {"MergeTensors","ContractIndices","MultiplyTensors","AddTensors"}

}
