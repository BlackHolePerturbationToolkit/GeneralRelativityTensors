{
  "More Information" -> {
      "The metric name n can be a two element List. The first element is the actual \"Name\" (used for internal purposes) \
while the second is the \"DisplayName\" which is used for formatted output in a notebook."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "TensorValues[gK] // TableForm",
    "MatrixForm[
    newMetVals = {{-A[r[x, y]], 0, 0}, {0, 1 + F[r[x, y]] x^2/r[x, y]^2,
    x y F[r[x, y]]/r[x, y]^2}, {0, x y F[r[x, y]]/r[x, y]^2,
    1 + F[r[x, y]] y^2/r[x, y]^2}}]",
    "ToMetric[{\"g3\", \"\!\(\*SuperscriptBox[\(g\), \(new\)]\)\"}, {t, x, y}, newMetVals, \"Latin\"]"
    },

    "See Also" ->
    {"ToTensor","ToCurve","ToTensorOnCurve","TensorValues"}

}
