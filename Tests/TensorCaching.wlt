gK = ToMetric["Kerr"];


VerificationTest[
	$CacheTensorValues,
	False,
	TestID->"$CacheTensorValues"	
];

VerificationTest[
	CachedTensorValues[gK],
	{},
	TestID->"CachedTensorValues1"	
];

$CacheTensorValues = True;
TensorValues[gK];

VerificationTest[
	CachedTensorValues[gK],
	{{"KerrMetric", {"Down", 
    "Down"}} -> {{(-a^2 + 2 M r - r^2 + a^2 Sin[\[Theta]]^2)/(
    r^2 + a^2 Cos[\[Theta]]^2), 0, 
    0, -((2 a M r Sin[\[Theta]]^2)/(
     r^2 + a^2 Cos[\[Theta]]^2))}, {0, (r^2 + a^2 Cos[\[Theta]]^2)/(
    a^2 - 2 M r + r^2), 0, 0}, {0, 0, r^2 + a^2 Cos[\[Theta]]^2, 
    0}, {-((2 a M r Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2)), 0, 
    0, (Sin[\[Theta]]^2 ((a^2 + r^2)^2 - 
       a^2 (a^2 - 2 M r + r^2) Sin[\[Theta]]^2))/(
    r^2 + a^2 Cos[\[Theta]]^2)}}},
	TestID->"CachedTensorValues2"	
];

$CacheTensorValues = False;

VerificationTest[
	CachedTensorValues[gK],
	{{"KerrMetric", {"Down", 
    "Down"}} -> {{(-a^2 + 2 M r - r^2 + a^2 Sin[\[Theta]]^2)/(
    r^2 + a^2 Cos[\[Theta]]^2), 0, 
    0, -((2 a M r Sin[\[Theta]]^2)/(
     r^2 + a^2 Cos[\[Theta]]^2))}, {0, (r^2 + a^2 Cos[\[Theta]]^2)/(
    a^2 - 2 M r + r^2), 0, 0}, {0, 0, r^2 + a^2 Cos[\[Theta]]^2, 
    0}, {-((2 a M r Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2)), 0, 
    0, (Sin[\[Theta]]^2 ((a^2 + r^2)^2 - 
       a^2 (a^2 - 2 M r + r^2) Sin[\[Theta]]^2))/(
    r^2 + a^2 Cos[\[Theta]]^2)}}},
	TestID->"CachedTensorValues3"
];

ClearCachedTensorValues[gK];

VerificationTest[
	CachedTensorValues[gK],
	{},
	TestID->"ClearCachedTensorValues"
]
