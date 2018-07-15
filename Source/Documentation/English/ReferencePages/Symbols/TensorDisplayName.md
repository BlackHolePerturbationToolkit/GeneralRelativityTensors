{
  "More Information" -> {
      "If no TensorDisplayName is given when a Tensor is formed, \
the TensorName will be used for the TensorDisplayName also.",
    "If a Tensor is on a Curve, it will be displayed with the curve parameter."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "TensorName[gS]",
    "TensorDisplayName[gS]",
    "xT = ToTensor[\"x1\",gS,{k[t],f[r],g[r],h[r]}]",
    "TensorName[xT]",
    "TensorDisplayName[xT]"
    },

    "See Also" ->
    {"TensorName"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors"
     }
}
