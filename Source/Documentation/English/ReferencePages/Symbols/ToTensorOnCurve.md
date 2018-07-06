{
  "More Information" -> {
      "By default the RawTensorValues of a Tensor on a curve are not evaluated on that curve internally. \
Thus, derivatives can still be taken with respect to coordinates.",
      "When TensorValues is called on a Tensor on a curve the internal RawTensorValues are evaluated \
along the curve.",
      "When setting the Option \"ParametrizedValues\" -> True ToTensorOnCurve expects vals to be \
given as parametrized values of the CurveParameter of c."

  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "c1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
    "gKC = ToTensorOnCurve[gK, c1]",
    "RawTensorValues[gKC]",
    "vals = TensorValues[gKC]",
    "gKC2 = ToTensorOnCurve[\"gCurve\", c1, vals, {-\[Alpha], -\[Beta]}, \"ParametrizedValues\" -> True]",
    "RawTensorValues[gKC2]"
    },

    "See Also" ->
    {"ToMetric","ToTensor","TensorValues","RawTensorValues","ToCurve","Curve","CurveQ","OnCurveQ","ParametrizedValuesQ"}

}
