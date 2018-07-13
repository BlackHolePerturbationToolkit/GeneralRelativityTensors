{
  "More Information" -> {
      "CachedTensorValues are stored as Values in the Symbol RawTensorValues \
using TensorName and IndexPositions as Keys.",
      "Simply re-setting $CacheTensorValues to False does not remove already cached values.",
      "Instead, call ClearCachedTensorValues."
  },

  "Basic Examples" -> {
    "$CacheTensorValues = True",
    "gK = ToMetric[\"Kerr\"]",
    "TensorValues[gK]",
    "CachedTensorValues[gK]",
    "$CacheTensorValues = False",
    "CachedTensorValues[gK]",
    "ClearCachedTensorValues[gK]",
    "CachedTensorValues[gK]"
    },

    "See Also" ->
    {"TensorValues","RawTensorValues","CachedTensorValues","ClearCachedTensorValues"}

}
