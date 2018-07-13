{
  "More Information" -> {
      "By default RawTensorValues are not cached. To do so, set $CacheTensorValues = True.",
      "CachedTensorValues are stored as Values in the Symbol RawTensorValues \
using TensorName and IndexPositions as Keys.",
      "TensorNames ending in \"-Auto\" are not cached."
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
    {"TensorValues","RawTensorValues","$CacheTensorValues","ClearCachedTensorValues"}

}
