{
  "More Information" -> {
      "Usually it is preferable to use ToTensorFieldOnCurve or ToTensorOnCurve to SetCurve."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "c1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
    "gKC = SetCurve[gK, c1]",
    "Curve[gKC]"
    },

    "See Also" ->
    {"SetTensorKeyValue","ToCurve","ToTensorOnCurve","ToTensorFieldOnCurve","Curve"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
