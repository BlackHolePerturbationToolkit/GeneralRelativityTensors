{
  "More Information" -> {
      "ToTensorOnCurve always returns a Tensor with internal values evaluated on the curve. \
Thus, derivatives cannot be taken with respect to coordinates.",
"For Tensors on Curves, TensorValues and RawTensorValues return the same values."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "c1 = ToCurve[\"x1\", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]]",
    "gKC = ToTensorOnCurve[gK, c1]",
    "RawTensorValues[gKC]",
    "TensorValues[gKC]"
    },

    "See Also" ->
    {"ToTensorFieldOnCurve","TensorValues","RawTensorValues","ToCurve","Curve","CurveQ","OnCurveQ"},

    "Tutorials" -> {
      "Introduction to Tensor Curves",
      "Built in common Tensors"
      }

}
