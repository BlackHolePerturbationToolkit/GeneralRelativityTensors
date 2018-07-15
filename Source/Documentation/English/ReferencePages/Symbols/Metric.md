{
  "More Information" -> {
      "Metric[t] will return t itself if it is a metric.",
   "If t is on a curve, Metric[t] returns the metric Tensor on the same curve."
  },

  "Basic Examples" -> {
      "gK = ToMetric[\"Kerr\"]",
      "c1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
      "gKC = ToTensorOnCurve[gK, c1]",
      "Metric[gK]",
      "Metric[c1]",
      "Metric[gKC]"
    },

    "See Also" ->
    {"ToMetric","MetricQ","ToCurve"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Introduction to Tensor Curves",
      "Built in common Tensors"
    }

}
