{
  "More Information" -> {
      "The metric name n can be a two element List. The first element is the actual \"Name\" (used for internal purposes) \
while the second is the \"DisplayName\" which is used for formatted output in a notebook.",
"Choices for builtIn are \"Minkowski\" (or \"Mink\"),\"MinkowskiSpherical\" (or \"MinkSph\") \
\"Schwarzschild\" (or \"Schw\"), \"Kerr\", \"ReissnerNordstrom\" \
  (or \"RN\"), \"KerrNewman\" (or \"KN\"), \"TwoSphere\" (or \"S2\"), \"SchwarzschildM2\" (or \"SchwM2\"), \
\"SchwarzschildS2\" (or \"SchwS2\"), \"ReissnerNordstromM2\" (or \"RNM2\"), \
  and \"ReissnerNordstromS2\" (or \"RNS2\").",
  "Possible indices inds can be \"Greek\", \"Latin\", \"CaptialLatin\" or a List of Symbols.",
  "Default coordinates for builtIn metrics are Schwarzschild or Boyer-Lindquist.",
  "Default indices for four built-in four-dimensional metrics are lower-case Greek.",
  "Default indices for the S2 sector are upper-case Latin.",
  "Default indices for the M2 sector are lower-case Latin."
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
