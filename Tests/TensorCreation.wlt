gK = ToMetric["Kerr"];

VerificationTest[
	TensorValues[gK],
	{{(-a^2 + 2 M r - r^2 + a^2 Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2), 0, 0, 
		-((2 a M r Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2))}, 
  	{0, (r^2 + a^2 Cos[\[Theta]]^2)/(a^2 - 2 M r + r^2), 0, 0}, 
  	{0, 0,  r^2 + a^2 Cos[\[Theta]]^2, 0}, 
  	{-((2 a M r Sin[\[Theta]]^2)/(r^2 + a^2 Cos[\[Theta]]^2)), 0, 0, 
  		(Sin[\[Theta]]^2 ((a^2 + r^2)^2 - a^2 (a^2 - 2 M r + r^2) Sin[\[Theta]]^2))/(r^2 + a^2 Cos[\[Theta]]^2)}},
	TestID->"ToMetric1"	
]

VerificationTest[
	TensorValues@ToTensor["t1", gK, {g[t], h[r], j[r, \[Theta]], k[r, \[Phi]]}],
	{g[t], h[r], j[r, \[Theta]], k[r, \[Phi]]},
	TestID->"ToTensor1"	
]

