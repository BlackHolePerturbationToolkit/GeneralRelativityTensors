{
  "More Information" -> {"CachedTensorValues are stored as Values in the Symbol RawTensorValues \
using TensorName and IndexPositions as Keys."
  },

  "Basic Examples" -> {
    "$CacheTensorValues = True",
    "gK = ToMetric[\"Kerr\"]",
    "TensorValues[gK]",
    "CachedTensorValues[gK]",
    "ClearCachedTensorValues[gK]",
    "CachedTensorValues[gK]",
    "$CacheTensorValues = False"
    },

    "See Also" ->
    {"TensorValues","RawTensorValues","$CacheTensorValues","CachedTensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors",
      "Caching Tensor values"
    }

}
