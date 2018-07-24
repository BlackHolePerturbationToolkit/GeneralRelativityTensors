{
  "More Information" -> {
      "There is nothing stopping you from performing unwise things, as in the example below."
  },

  "Basic Examples" -> {
    "ARN = MaxwellPotential[\"ReissnerNordstrom\"]",
    "Metric[ARN] // TensorName",
    "AS = SetMetric[ARN, ToMetric[\"Schwarzschild\"]]",
    "Metric[AS] // TensorName"
    },

    "See Also" ->
    {"SetTensorKeyValue","Metric","SetMetricQ"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
