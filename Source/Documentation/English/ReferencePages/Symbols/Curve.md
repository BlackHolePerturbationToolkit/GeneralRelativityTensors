{
  "More Information" -> {
      "Curve[t] returns Undefined if t is not on a curve.",
   "Curve[t] will return t itself if it is a curve."
  },

  "Basic Examples" -> {
      "gK = ToMetric[\"Kerr\"]",
      "c1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
      "gKC = ToTensorOnCurve[gK, c1]",
      "Curve[gK]",
      "Curve[c1]",
      "Curve[gKC]"
    },

    "See Also" ->
    {"CurveQ","ToCurve","ToTensorOnCurve"}

}
