{
  "More Information" -> {
      "It is generally simpler to call MergeTensors than to call SumTensors (which MergeTensors does internally)."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "SumTensors[gS, gS]",
    "twoGSa = SumTensors[gS, gS, {\"2SchwarzschildMetricA\", \"2g\"}]",
    "twoGSb = MultiplyTensorScalar[2, gS, {\"2SchwarzschildMetricB\", \"2g\"}]",
    "TensorValues[twoGS] - TensorValues[twoGSb] // Simplify"
    },

    "See Also" ->
    {"MergeTensors","ContractIndices","MultiplyTensors","MultiplyTensorScalar"}

}
