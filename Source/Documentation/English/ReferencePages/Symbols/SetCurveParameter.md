{
  "More Information" -> {
  },

  "Basic Examples" -> {
      "gK = ToMetric[\"Kerr\"]",
    "x1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
    "CurveParameter[x1]",
    "x2 = SetCurveParameter[x1, \[Tau]]",
    "CurveParameter[x2]"
    },

    "See Also" ->
    {"SetTensorKeyValue","ToCurve","CurveParameter"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
