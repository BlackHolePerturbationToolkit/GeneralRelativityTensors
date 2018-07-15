{
  "More Information" -> {
      "CachedTensorValues are stored as Values in the Symbol RawTensorValues \
    using TensorName and IndexPositions as Keys.",
      "With caching on, RawTensorValues is not updated.",
      "Unless a Tensor is a field with an associated Curve, RawTensorValues and \
TensorValues return the same values."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "RawTensorValues[gS]",
    "uS = FourVelocityVector[\"SchwarzschildGeneric\"]",
    "RawTensorValues[uS]",
    "gSF = ToTensorFieldOnCurve[gS,Curve[uS]]",
    "RawTensorValues[gSF]",
    "TensorValues[gSF]"
    },

    "See Also" ->
    {"TensorValues","CachedTensorValues","ClearCachedTensorValues","$CacheTensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Introduction to Tensor Curves",
      "Caching Tensor values"
    }

}
