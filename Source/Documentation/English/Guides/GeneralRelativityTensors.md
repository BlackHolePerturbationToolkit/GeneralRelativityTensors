{
 "Application" -> "GeneralRelativityTensors",
 "Package" -> "GeneralRelativityTensors",
 "Title" -> "GeneralRelativityTensors: An application for tensor calculations",
 "Summary" -> "GeneralRelativityTensors: An application for tensor calculations",
 "Description" ->
   {"The GeneralRelativityTensors application provides functions for ",
    "coordinate based calculations, particularly in the context of black holes."},
 "Keywords" -> {"GeneralRelativityTensors"},
 "Label" -> "Tensors Application",
 "Synonyms" -> {"Tensors"},
 "URL" -> "http://bitbucket.org/hoppese/GeneralRelativityTensors",
 "Packages" -> {
   {"Title" -> "Tensor creation and common functions",
    "DetailedFunctions" -> {
      {"Tensor", "Head for tensors in GeneralRelativityTensors"},
      {"ToTensor", "Form a new Tensor"},
      {"ToMetric", "Form a new metric Tensor"},
      {"ToCurve", "Form a new Curve Tensor"},
      {"ToTensorFieldOnCurve", "Form a new Tensor with an associated Curve"},
      {"ToTensorOnCurve", "Form a new Tensor on a Curve"},
      {"RawTensorValues", "Values stored internally by Tensor"},
      {"TensorValues", "Values stored internally by Tensor, potentially evaluated on a Curve"},
      {"Rank", "Rank of Tensor"},
      {"Indices", "List of Indices of Tensor"},
      {"PossibleIndices", "List of all possible indices of Tensor"},
      {"IndexPositions","List of elements \"Up\" and \"Down\" giving the positions of Tensor indices"},
      {"Dimensions", "Dimensions is overloaded. For GeneralRelativityTensors it gives the number of dimensions of the manifold."},
      {"Coordinates", "List of the coordinates of the Tensor"},
      {"TensorRules", "List of Rules showing Tensor values for given components"},
      {"TensorName", "Internal name of Tensor"},
      {"TensorDisplayName", "Name used for display of Tensor in notebook"},
      {"Metric", "Metric associated with the Tensor"},
      {"InverseMetric", "Inverse metric associated with the Tensor"},
      {"Curve", "Curve associated with the Tensor"},
      {"MetricQ", "Test whether a Tensor is a metric"},
      {"CurveQ", "Test whether a Tensor is a Curve"},
      {"OnCurveQ", "Test whether a Tensor is on a Curve"},
      {"ValidTensorExpressionQ", "Test whether a Tensor expression is valid (in indices and metrics)"},
      {"ValidateTensorExpression", "Similar to ValidTensorExpressionQ, but aborts and prints error messages"}
    }
   },
   {"Title" -> "Tensor manipulation",
    "DetailedFunctions" -> {
      {"ShiftIndices", "Raise and lower indices on a Tensor"},
      {"ActOnTensorValues", "Apply a function to the values of a Tensor"},
      {"MergeTensors", "Combine an expression into a single Tensor"},
      {"ContractIndices", "Trace over repeated indices"},
      {"MultiplyTensors", "Outer product of Tensors"},
      {"MultiplyTensorScalar", "Product of a Tensor and a scalar"},
      {"AddTensors", "Sum of Tensors"},
      {"D","D is overloaded. It takes the partial derivative of a Tensor."},
      {"CovariantD", "Covariant derivative of a Tensor expression"},
      {"TensorPattern", "A Tensor with Patterns for indices for matching and replacements"}
    }
   },
   {"Title" -> "Low level operators",
    "DetailedFunctions" -> {
      {"SetTensorKeyValue", "Change an internal key-value pair"},
      {"SetTensorName", "Change internal name"},
      {"SetTensorDisplayName", "Change string used for display"},
      {"SetTensorValues", "Change internal values"},
      {"SetMetric", "Change the Tensor's associated metric"},
      {"SetMetricQ", "Change internal MetricQ flag"},
      {"SetCurve", "Change the Tensor's associated Curve"},
      {"SetCurveParameter", "Change the Tensor's associated CurveParameter"},
      {"SetCurveQ", "Change internal CurveQ flag"},
      {"SetCoordinates", "Change internal coordinates"},
      {"SetIndices", "Change internal indices"},
      {"SetPossibleIndices", "Change internal list of possible indices"},
      {"SetDimensions", "Change internal value of dimensions"}
    }
   },
   {"Title" -> "Common Tensors and functions",
    "DetailedFunctions" -> {
      {"ChristoffelSymbol", "Connection coefficients for a metric"},
      {"RiemannTensor", "Riemann tensor for a metric"},
      {"RicciTensor", "Ricci tensor for a metric"},
      {"RicciScalar", "Ricci scalar for a metric"},
      {"EinsteinTensor", "Einstein tensor for a metric"},
      {"WeylTensor", "Weyl Tensor for a metric"},
      {"CottonTensor", "Cotton Tensor for a metric"},
      {"KretschmannScalar", "Kretschmann scalar for a metric"},
      {"MaxwellPotential", "Electromagnetic four vector potential"},
      {"FieldStrengthTensor","Field strength tensor for a vector potential"},
      {"MaxwellStressEnergyTensor","Stress energy tensor for a field strength tensor"},
      {"FourVelocityVector","Four-velocity for common spacetimes"},
      {"KinnersleyNullVector","Null vector common in Newman-Penrose calculations"},
      {"KinnersleyNullTetrad","The four Kinnersley null vectors"},
      {"KinnersleyDerivative","Derivative associated with a Kinnersley null vector"},
      {"SpinCoefficient","One of 12 Newman-Penrose spin coefficients"},
      {"BianchiIdentities", "Bianchi identities for a metric"}
    }
   },
   {"Title" -> "Caching Tensor values",
    "DetailedFunctions" -> {
      {"$CacheTensorValues", "Global variable for caching"},
      {"CachedTensorValues", "Values that are cached internally"},
      {"ClearCachedTensorValues", "Remove internally cached values"}
     }
   }
  },
 "Tutorials" -> {
   "Introduction to GeneralRelativityTensors",
   "Introduction to Tensor Curves",
   "Manipulating and differentiating Tensors",
   "Low level Tensor operations",
   "Built in common Tensors",
   "Caching Tensor values",
   "Pattern matching with Tensors",
   "Examples - Wave equations"
 }
}
