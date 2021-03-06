gK = ToMetric["Kerr"];
c1 = ToCurve["x1", gK, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]];
gKC = ToTensorFieldOnCurve[gK, c1];
gKC2 = ToTensorOnCurve["gCurve", c1, TensorValues[gKC], {-\[Alpha], -\[Beta]}];

VerificationTest[
	TensorValues[c1],
	{t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]},
	TestID->"Curve1"	
]

VerificationTest[
	RawTensorValues[gKC]
	,
	{{(-a^2 + 2 M r - r^2 + a^2 Sin[\[Theta]]^2)/(
  r^2 + a^2 Cos[\[Theta]]^2), 0, 
  0, -((2 a M r Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2))}, {0, (
  r^2 + a^2 Cos[\[Theta]]^2)/(a^2 - 2 M r + r^2), 0, 0}, {0, 0, 
  r^2 + a^2 Cos[\[Theta]]^2, 
  0}, {-((2 a M r Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2)), 0, 
  0, (Sin[\[Theta]]^2 ((a^2 + r^2)^2 - 
     a^2 (a^2 - 2 M r + r^2) Sin[\[Theta]]^2))/(
  r^2 + a^2 Cos[\[Theta]]^2)}},
	TestID->"Curve-RawTensorValues"	
]

VerificationTest[
	TensorValues[gKC],
	{{((1 + e Cos[\[Chi]])^2 (-((M^2 p^2)/(1 + e Cos[\[Chi]])^2) + (
     2 M^2 p)/(1 + e Cos[\[Chi]])))/(M^2 p^2), 0, 
  0, -((2 a (1 + e Cos[\[Chi]]))/p)}, {0, (
  M^2 p^2)/((1 + e Cos[\[Chi]])^2 (a^2 + (
     M^2 p^2)/(1 + e Cos[\[Chi]])^2 - (2 M^2 p)/(1 + e Cos[\[Chi]]))),
   0, 0}, {0, 0, (M^2 p^2)/(1 + e Cos[\[Chi]])^2, 
  0}, {-((2 a (1 + e Cos[\[Chi]]))/p), 0, 
  0, ((1 + e Cos[\[Chi]])^2 ((a^2 + (
       M^2 p^2)/(1 + e Cos[\[Chi]])^2)^2 - 
     a^2 (a^2 + (M^2 p^2)/(1 + e Cos[\[Chi]])^2 - (2 M^2 p)/(
        1 + e Cos[\[Chi]]))))/(M^2 p^2)}},
	TestID->"Curve-TensorValues"	
]

VerificationTest[
	Curve[c1],
	c1,
	TestID->"Curve-Curve1"	
]

VerificationTest[
	Curve[gKC],
	c1,
	TestID->"Curve-Curve2"	
]

VerificationTest[
	Curve[gK],
	Undefined,
	TestID->"Curve-Curve3"	
]

VerificationTest[
	CurveQ[c1],
	True,
	TestID->"Curve-CurveQ1"	
]

VerificationTest[
	CurveQ[gKC],
	False,
	TestID->"Curve-CurveQ2"	
]

VerificationTest[
	OnCurveQ[c1],
	True,
	TestID->"Curve-OnCurveQ1"	
]

VerificationTest[
	OnCurveQ[gKC],
	False,
	TestID->"Curve-OnCurveQ2"	
]

VerificationTest[
	OnCurveQ[gKC2],
	True,
	TestID->"Curve-OnCurveQ3"	
]

