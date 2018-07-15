{
  "More Information" -> {
      "The Tensor returned by ToTensorFieldOnCurve has internal values that are not evaluated on the Curve. \
Thus, derivatives can still be taken with respect to coordinates.",
      "When TensorValues is called on a Tensor with an associated Curve the internal RawTensorValues are evaluated \
along the curve."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "c1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
    "gKF = ToTensorFieldOnCurve[gK, c1]",
    "RawTensorValues[gKF]",
    "TensorValues[gKF]"
    },

    "See Also" ->
    {"ToTensorOnCurve","TensorValues","RawTensorValues","ToCurve","Curve","CurveQ","OnCurveQ"},

    "Tutorials" -> {
      "Introduction to Tensor Curves",
      "Built in common Tensors"
      }

}
