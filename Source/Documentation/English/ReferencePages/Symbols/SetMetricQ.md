{
  "More Information" -> {
      "When setting MetricQ to False, make sure to give the Tensor a new metric with SetMetric."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "MetricQ[gK]",
    "gK1 = SetMetricQ[gK, False]",
    "MetricQ[gK1]",
    "gK2 = SetMetric[gK1, ToMetric[\"Schwarzschild\"]]",
    "Metric[gK2] //TensorName"
    },

    "See Also" ->
    {"SetTensorKeyValue","ToMetric","Metric","MetricQ","SetMetric"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
