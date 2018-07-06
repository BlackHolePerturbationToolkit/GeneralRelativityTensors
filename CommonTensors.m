(* ::Package:: *)

BeginPackage["Tensors`CommonTensors`",
			{"Tensors`TensorDefinitions`",
			 "Tensors`TensorDerivatives`",
			 "Tensors`TensorManipulation`"}];


RiemannTensor::usage="RiemannTensor[m] returns the Riemann Tensor with \
index positions {\"Up\",\"Down\",\"Down\",\"Down\"} computed from the metric Tensor m.";
RicciTensor::usage="RicciTensor[m] returns the Ricci Tensor with index \
positions {\"Down\",\"Down\"} computed from the metric Tensor m.";
RicciScalar::usage="RicciScalar[m] returns the Ricci scalar computed from \
the metric Tensor m.";
EinsteinTensor::usage="EinsteinTensor[m] returns the Einstein Tensor with \
index positions {\"Down\",\"Down\"} computed from the metric Tensor m.";
WeylTensor::usage="WeylTensor[m] returns the Weyl Tensor with index positions \
{\"Down\",\"Down\",\"Down\",\"Down\"} computed from the metric Tensor m.";

KinnersleyNullVector::usage="KinnersleyNullVector[m,v] returns \
the contravariant Kinnersley null vector associated with metric Tensor m and \
string v, where v can be \"l\", \"n\", \"m\", or \"mStar\".
KinnersleyNullVector[builtIn,v] is equivalent to \
KinnersleyNullVector[ToMetric[builtIn],v], where \
builtIn can be \"Schwarzschild\" or \"Kerr\"."
KinnersleyNullTetrad::usage="KinnersleyNullTetrad[m] returns a list of the four \
KinnersleyNullVector in order {\"l\", \"n\", \"m\", \"mStar\"} for the metric m.
KinnersleyNullTetrad[builtIn] is equivalent to \
KinnersleyNullTetrad[ToMetric[builtIn]], where builtIn \
can be \"Schwarzschild\" or \"Kerr\"."
KinnersleyDerivative::usage="KinnersleyDerivative[m,s] returns the projected \
derivative s being the appropriate Kinnersley null vector contracted with a partial derivative. Values for \
s are \"D\", \"Delta\", \"delta\", or \"deltaStar\".
KinnersleyDerivative[builtIn,s] is equivalent to \
KinnersleyDerivative[ToMetric[builtIn],s], \
where builtIn can be \"Schwarzschild\" or \"Kerr\"."
SpinCoefficient::usage="SpinCoefficient[s] returns the Newman-Penrose \
spin coefficient corresponding to the string s, where possible values of \
s are \"alpha\",\"beta\",\"gamma\",\"epsilon\",\"kappa\",\"lambda\",\
\"mu\",\"nu\",\"pi\",\"rho\",\"sigma\", and \"tau\".";

KretschmannScalar::usage="KretschmannScalar[m] returns the \
Kretschmann scalar (Riemann tensor squared) associated with the metric m.";
BianchiIdentities::usage="BianchiIdentities[m,n] returns the \
n-th contracted Bianchi identities, where \
n can be 0, 1, or 2. BianchiIdentities[m] is equivalent to BianchiIdentities[m,0].";

MaxwellPotential::usage="MaxwellPotential[builtIn] returns the four-vector A on a built-in background. \
The only current choice for builtIn is \"ReissnerNordstrom\" (or \"RN\").";
FieldStrengthTensor::usage="FieldStrengthTensor[A] returns the field strength tensor associated with the \
electromagnetic vector potential A.";
MaxwellStressEnergyTensor::usage="MaxwellStressEnergyTensor[F] returns the stress energy tensor associated with the \
electromagnetic field strength tensor F.";

FourVelocity::usage="FourVelocity[builtIn] returns the four velocity associated with the string builtIn. \
Choices are \"KerrGeneric\" and \"SchwarzschildGeneric\".";

LeviCivitaSymbol::usage="LeviCivitaSymbol[builtIn] returns the Levi-Civita symbol associated with the 
built-in spacetime. The only current choice for builtIn is \"TwoSphere\" (or \"S2\").";
TensorSphericalHarmonic::usage="TensorSphericalHarmonic[builtIn] returns a Martel-Poisson tensor spherical harmonic \
associated with the string builtIn. Choices are \"YA\", \"XA\", \"YAB\", and \"XAB\".";
M2Amplitude::usage="M2Amplitude[builtIn] returns a Martel-Poisson metric perturbation amplitude \
associated with the string builtIn. Choices are \"hab\", \"ha\", and \"ja\".";


Begin["`Private`"];


Options[RiemannTensor]=Options[ChristoffelSymbol];
Options[RicciTensor]=Options[ChristoffelSymbol];
Options[RicciScalar]=Options[ChristoffelSymbol];
Options[EinsteinTensor]=Options[ChristoffelSymbol];
Options[WeylTensor]=Options[ChristoffelSymbol];
Options[FieldStrengthTensor]=Options[ChristoffelSymbol];
Options[MaxwellStressEnergyTensor]=Options[ChristoffelSymbol];
Options[KretschmannScalar]=Options[ChristoffelSymbol];
Options[BianchiIdentities]=Options[ChristoffelSymbol];
Options[KinnersleyNullTetrad]=Options[KinnersleyNullVector];
Options[SpinCoefficient]={"Conjugate"->False,"Schwarzschild"->False};

DocumentationBuilder`OptionDescriptions["RiemannTensor"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["RicciTensor"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["RicciScalar"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["EinsteinTensor"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["WeylTensor"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["FieldStrengthTensor"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["MaxwellStressEnergyTensor"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["KretschmannScalar"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["BianchiIdentities"] = DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"];
DocumentationBuilder`OptionDescriptions["SpinCoefficient"] ={"Conjugate"->"Boolean stating whether to return the complex \
conjugate of the spin coefficient",
"Schwarzschild"->"Boolean stating whether to return the spin coefficient for Schwarzschild spacetime (as opposed to Kerr)"};


ToMetric["Minkowski"]:=
Module[{t,x,y,z,\[Alpha],\[Beta]},	

	{t,x,y,z,\[Alpha],\[Beta]}=Symbol/@{"t","x","y","z","\[Alpha]","\[Beta]"};

	ToMetric[Association["Name"->"MinkowskiMetric",
				"Coordinates"->{t,x,y,z},
				"DisplayName"->"\[Eta]",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];
ToMetric["Mink"]:=ToMetric["Minkowski"];


ToMetric["MinkowskiSpherical"]:=
Module[{t,r,\[Theta],\[Phi],\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","\[Alpha]","\[Beta]"};

	ToMetric[Association["Name"->"MinkowskiMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"\[Eta]",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1,0,0,0},{0,1,0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];
ToMetric["MinkSph"]:=ToMetric["MinkowskiSpherical"];


ToMetric["Schwarzschild"]:=
Module[{t,r,\[Theta],\[Phi],M,\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],M,\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","\[Alpha]","\[Beta]"};
	
	ToMetric[Association["Name"->"SchwarzschildMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"g",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1+(2 M)/r,0,0,0},{0,1/(1-(2 M)/r),0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];
ToMetric["Schw"]:=ToMetric["Schwarzschild"]


ToMetric["SchwarzschildM2"]:=
Module[{t,r,M,a,b},	

	{t,r,M,a,b}=Symbol/@{"t","r","M","a","b"};
	
	ToMetric[Association["Name"->"SchwarzschildM2Metric",
				"Coordinates"->{t,r},
				"DisplayName"->"g",
				"Indices"->{-a,-b},
				"PossibleIndices"->"Latin",
				"Abstract"->False,
				"Values"->{{-1+(2 M)/r,0},{0,1/(1-(2 M)/r)}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];
ToMetric["SchwM2"]:=ToMetric["SchwarzschildM2"];


ToMetric["SchwarzschildS2"]:=
Module[{th,ph,r},
	{th,ph,r}=Symbol/@{"\[Theta]","\[Phi]","r"};
	ToMetric[{"SchwarzschildS2Metric","g"},{th,ph},r^2 RawTensorValues[ToMetric["TwoSphere"]],"CapitalLatin"]
];
ToMetric["SchwS2"]:=ToMetric["SchwarzschildS2"];


ToMetric["Kerr"]:=
Module[{t,r,\[Theta],\[Phi],M,a,\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],M,a,\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","a","\[Alpha]","\[Beta]"};

	ToMetric[Association["Name"->"KerrMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"g",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{(-a^2+2 M r-r^2+a^2 Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2),0,0,-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2))},
							{0,(r^2+a^2 Cos[\[Theta]]^2)/(a^2-2 M r+r^2),0,0},
							{0,0,r^2+a^2 Cos[\[Theta]]^2,0},
							{-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2)),0,0,(Sin[\[Theta]]^2 ((a^2+r^2)^2-a^2 (a^2-2 M r+r^2) Sin[\[Theta]]^2))/(r^2+a^2 Cos[\[Theta]]^2)}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];


ToMetric["TwoSphere"]:=
Module[{th,ph},
	{th,ph}=Symbol/@{"\[Theta]","\[Phi]"};
	ToMetric[{"TwoSphereMetric","\[CapitalOmega]"},{th,ph},{{1,0},{0,Sin[th]^2}},"CapitalLatin"]
];
ToMetric["S2"]:=ToMetric["TwoSphere"];


ToMetric["ReissnerNordstrom"]:=
Module[{t,r,\[Theta],\[Phi],M,Q,\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],M,Q,\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","Q","\[Alpha]","\[Beta]"};
	
	ToMetric[Association["Name"->"ReissnerNordstromMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"g",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1+(2 M)/r-Q^2/r^2,0,0,0},{0,1/(1-(2 M)/r+Q^2/r^2),0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];
ToMetric["RN"]:=ToMetric["ReissnerNordstrom"];


ToMetric["ReissnerNordstromM2"]:=
Module[{t,r,M,Q,a,b},	

	{t,r,M,Q,a,b}=Symbol/@{"t","r","M","Q","a","b"};
	
	ToMetric[Association["Name"->"ReissnerNordstromM2Metric",
				"Coordinates"->{t,r},
				"DisplayName"->"g",
				"Indices"->{-a,-b},
				"PossibleIndices"->"Latin",
				"Abstract"->False,
				"Values"->{{-1+(2 M)/r-Q^2/r^2,0},{0,1/(1-(2 M)/r+Q^2/r^2)}},
				"CurveParameter"->Undefined,
				"ParametrizedValues"->False,
				"Curve"->Undefined,
				"IsCurve"->False]]
];
ToMetric["RNM2"]:=ToMetric["ReissnerNordstromM2"]


ToMetric["ReissnerNordstromS2"]:=SetTensorName[ToMetric["SchwarzschildS2"],{"ReissnerNordstromS2Metric","g"}];
ToMetric["RNS2"]:=ToMetric["ReissnerNordstromS2"];


Clear[LeviCivitaSymbol]
LeviCivitaSymbol["TwoSphere"]:=
Module[{th,ph,A,B},
	{th,ph,A,B}=Symbol/@{"\[Theta]","\[Phi]","A","B"};
	ToTensor[{"LeviCivitaSymbol","\[CurlyEpsilon]"},ToMetric["TwoSphere"],{{0,Sin[th]},{-Sin[th],0}},{-A,-B}]
];
LeviCivitaSymbol["S2"]:=LeviCivitaSymbol["TwoSphere"]


Clear[RiemannTensor]
Tensor/:RiemannTensor[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,g,ig,xx,chr,vals,posInds,gT,name,simpFn},
	simpFn=OptionValue["SimplifyFunction"];
	gT=Metric[t];
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	g=RawTensorValues[gT];
	ig=RawTensorValues@InverseMetric[gT];
	chr=RawTensorValues@ChristoffelSymbol[gT,"SimplifyFunction"->simpFn];

	g=(Association@@Metric[t])["Values"];
	ig=(Association@@InverseMetric[t])["Values"];
	chr=(Association@@ChristoffelSymbol[gT,"SimplifyFunction"->simpFn])["Values"];

	name="RiemannTensor"<>TensorName[t];

	vals=
		If[RawTensorValues[name,{"Up","Down","Down","Down"}]===Undefined,
			simpFn@Table[D[chr[[i,k,m]],xx[[l]]]-D[chr[[i,k,l]],xx[[m]]]
						+Sum[chr[[i,s,l]]chr[[s,k,m]],{s,1,n}]
						-Sum[chr[[i,s,m]]chr[[s,k,l]],{s,1,n}],
							{i,1,n},{k,1,n},{l,1,n},{m,1,n}],
			RawTensorValues[name,{"Up","Down","Down","Down"}]
		];

	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","IsMetric","Indices"}],
		Association["Metric"->gT,
					"IsMetric"->False,
					"Values"->vals,
					"DisplayName"->"R",
					"Name"->name,
					"Indices"->{posInds[[1]],-posInds[[2]],-posInds[[3]],-posInds[[4]]}]]]
]


Clear[RicciTensor]
Tensor/:RicciTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,simpFn,name,posInds},

	simpFn=OptionValue["SimplifyFunction"];
	rie=RiemannTensor[g,"SimplifyFunction"->simpFn];
	name="RicciTensor"<>TensorName[g];
	posInds=PossibleIndices[rie];
	
	If[RawTensorValues[name,{"Down","Down"}]===Undefined,
		ActOnTensorValues[ContractIndices[rie[posInds[[1]],-posInds[[2]],-posInds[[1]],-posInds[[4]]],{name,"R"}],simpFn],
		ToTensor[Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","IsMetric","Indices"}],
						Association["Metric"->g,
									"IsMetric"->False,
									"Values"->RawTensorValues[name,{"Down","Down"}],
									"DisplayName"->"R",
									"Name"->name,
									"Indices"->{-posInds[[1]],-posInds[[2]]}]]]
	]		
]


Clear[RicciScalar]
Tensor/:RicciScalar[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{ric,posInds,simpFn,name},

	simpFn=OptionValue["SimplifyFunction"];
	ric=RicciTensor[g,"SimplifyFunction"->simpFn];
	name="RicciScalar"<>TensorName[g];
	posInds=PossibleIndices[ric];
	
	If[RawTensorValues[name,{}]===Undefined,
		ActOnTensorValues[ContractIndices[ric[-posInds[[1]],posInds[[1]]],{name,"R"}],simpFn],
		ToTensor[Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","IsMetric","Indices"}],
					Association["Metric"->g,
								"IsMetric"->False,
								"Values"->RawTensorValues[name,{}],
								"DisplayName"->"R",
								"Name"->name,
								"Indices"->{}]]]
	]		

]


Clear[EinsteinTensor]
Tensor/:EinsteinTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{ricT,ricS,simpFn,name,posInds},
	simpFn=OptionValue["SimplifyFunction"];
	ricT=RicciTensor[g,"SimplifyFunction"->simpFn];
	ricS=RicciScalar[g,"SimplifyFunction"->simpFn];
	posInds=PossibleIndices[ricT];
		
	name="EinsteinTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Down","Down"}]===Undefined,
		ActOnTensorValues[MergeTensors[ricT[-posInds[[1]],-posInds[[2]]]-1/2 ricS g[-posInds[[1]],-posInds[[2]]],{name,"G"}],simpFn],
		ToTensor[Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","IsMetric","Indices"}],
					Association["Metric"->g,
								"IsMetric"->False,
								"Values"->RawTensorValues[name,{"Down","Down"}],
								"DisplayName"->"G",
								"Name"->name,
								"Indices"->{-posInds[[1]],-posInds[[2]]}]]]
	]		
]


Clear[WeylTensor]
Tensor/:WeylTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,ricT,ricS,simpFn,dim,i,k,l,m,name},

	dim = Dimensions[g];
	If[dim <= 2, Print["Weyl tensor requires dimensions of at least 3"]; Abort[]];

	simpFn=OptionValue["SimplifyFunction"];
	rie=RiemannTensor[g,"SimplifyFunction"->simpFn];
	ricT=RicciTensor[g,"SimplifyFunction"->simpFn];
	ricS=RicciScalar[g,"SimplifyFunction"->simpFn];
	{i,k,l,m}=Take[PossibleIndices[g],4];
	name = "WeylTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Down","Down","Down","Down"}]===Undefined,
		ActOnTensorValues[
			MergeTensors[rie[-i,-k,-l,-m]+
				1/(dim-2) (ricT[-i,-m]g[-k,-l]-ricT[-i,-l]g[-k,-m]+ricT[-k,-l]g[-i,-m]-ricT[-k,-m]g[-i,-l])
				+ricS/((dim-1)(dim-2)) (g[-i,-l]g[-k,-m]-g[-i,-m]g[-k,-l]),{name,"C"}],simpFn],
		ToTensor[Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","IsMetric","Indices"}],
					Association["Metric"->g,
								"IsMetric"->False,
								"Values"->RawTensorValues[name,{"Down","Down","Down","Down"}],
								"DisplayName"->"C",
								"Name"->name,
								"Indices"->{-i,-k,-l,-m}]]]
	]
]


Clear[MaxwellPotential]
MaxwellPotential["ReissnerNordstrom"]:=
Module[{QQ,r,ind,met},
	met=ToMetric["ReissnerNordstrom"];
	ind=PossibleIndices[met][[1]];
	{QQ,r}=Symbol/@{"Q","r"};
	ToTensor[{"MaxwellPotential"<>TensorName[met],"A"},met,{QQ/r,0,0,0},{-ind}]
];
MaxwellPotential["RN"]:=MaxwellPotential["ReissnerNordstrom"];


Clear[FieldStrengthTensor]
Tensor/:FieldStrengthTensor[AA_Tensor,opts:OptionsPattern[]]:=
Module[{g,simpFn,name,posInds},
	If[Total@Rank[AA]=!=1,Print["Field strength tensor must be derived from a Rank 1 tensor"];Abort[]];
	If[AbstractQ[AA],Print["Field strength tensor requires a non-abstract potential"];Abort[]];
	simpFn=OptionValue["SimplifyFunction"];
	g=Metric[AA];	
	posInds=PossibleIndices[g];
	name="FieldStrengthTensor"<>TensorName[g];
	MergeTensors[CovariantD[AA[-posInds[[1]]],-posInds[[2]]]-CovariantD[AA[-posInds[[2]]],-posInds[[1]]],{name,"F"},"SimplifyFunction"->simpFn]
];
FieldStrengthTensor[str_String,opts:OptionsPattern[]]:=FieldStrengthTensor[MaxwellPotential[str],opts];


Clear[MaxwellStressEnergyTensor]
Tensor/:MaxwellStressEnergyTensor[FF_Tensor,opts:OptionsPattern[]]:=
Module[{g,simpFn,name,posInds},
	If[Total@Rank[FF]=!=2,Print["Maxwell stress energy tensor must be derived from a Rank 2 tensor"];Abort[]];
	If[AbstractQ[FF],Print["Maxwell stress energy requires a non-abstract field strength tensor"];Abort[]];
	simpFn=OptionValue["SimplifyFunction"];
	g=Metric[FF];	
	posInds=PossibleIndices[g];
	name="MaxwellStressEnergyTensor"<>TensorName[g];
	MergeTensors[1/(4\[Pi]) (FF[posInds[[1]],-posInds[[3]]]FF[posInds[[2]],posInds[[3]]]-1/4 g[posInds[[1]],posInds[[2]]]FF[-posInds[[3]],-posInds[[4]]]FF[posInds[[3]],posInds[[4]]]),{name,"T"},"SimplifyFunction"->simpFn]
];
MaxwellStressEnergyTensor[str_String,opts:OptionsPattern[]]:=MaxwellStressEnergyTensor[FieldStrengthTensor[MaxwellPotential[str],opts],opts];


Clear[KretschmannScalar]
Tensor/:KretschmannScalar[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,simpFn,name,is},
	simpFn=OptionValue["SimplifyFunction"];
	rie=RiemannTensor[g,"SimplifyFunction"->simpFn];
	is=PossibleIndices[rie];
		
	name="KretschmannScalar"<>TensorName[g];
	
	If[RawTensorValues[name,{}]===Undefined,
		ActOnTensorValues[MergeTensors[rie[is[[1]],is[[2]],is[[3]],is[[4]]]rie[-is[[1]],-is[[2]],-is[[3]],-is[[4]]],{name,"K"}],simpFn],
		ToTensor[Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","IsMetric","Indices"}],
					Association["Metric"->g,
								"IsMetric"->False,
								"Values"->RawTensorValues[name,{}],
								"DisplayName"->"K",
								"Name"->name,
								"Indices"->{}]]]
	]		
]


Clear[BianchiIdentities]
Tensor/:BianchiIdentities[t_Tensor?MetricQ,contractions_:0,opts:OptionsPattern[]]/;MemberQ[{0,1,2},contractions]:=
Module[{rie,ric,ein,simp,is},
	simp=OptionValue["SimplifyFunction"];
	
	is=PossibleIndices[t];
	rie=RiemannTensor[t,"SimplifyFunction"->simp];
	ric=RicciTensor[t,"SimplifyFunction"->simp];
	ein=EinsteinTensor[t,"SimplifyFunction"->simp];

	Switch[contractions,
		0,
		CovariantD[rie[-is[[1]],-is[[2]],-is[[3]],-is[[4]]],-is[[5]]]
		+CovariantD[rie[-is[[1]],-is[[2]],-is[[5]],-is[[3]]],-is[[4]]]
		+CovariantD[rie[-is[[1]],-is[[2]],-is[[4]],-is[[5]]],-is[[3]]],
		1,
		CovariantD[ric[-is[[2]],-is[[4]]],-is[[5]]]
		-CovariantD[ric[-is[[2]],-is[[5]]],-is[[4]]]
		+CovariantD[rie[is[[3]],-is[[2]],-is[[4]],-is[[5]]],-is[[3]]],
		2,
		CovariantD[ein[-is[[2]],-is[[4]]],is[[2]]]
	]
]


Clear[KinnersleyNullVector]
KinnersleyNullVector[t_Tensor?MetricQ,vec_String]:=
Module[{r,a,th,M,val,delta,sigma,valC,schw,rules},
	
	schw=TensorName[t]==="SchwarzschildMetric";

	{r,th,a,M}=Symbol/@{"r","\[Theta]","a","M"};
	sigma=r^2+a^2 Cos[th]^2;
	delta=a^2-2 M r+r^2;
	rules=If[schw,{a->0},{}];

	val=
	Switch[vec,
			"l",
			{(r^2+a^2)/delta,1,0,a/delta},
		
			"n",
			{r^2+a^2,-delta,0,a}/(2sigma),
	
			"m"|"mStar",
			{I a Sin[th],0,1,I/Sin[th]}/(Sqrt[2](r+I a Cos[th])),

			___,
			Print["No KinnersleyNullVector = "<>vec];
			Print["Options are \"l\", \"n\", \"m\", and \"mStar\"."];
			Abort[]
		]/.rules;

	valC=If[vec==="mStar",Simplify@ComplexExpand@Conjugate@#,#]&@val;

	ToTensor[{vec<>"Kinnersley"<>TensorName[t],If[vec==="mStar",\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SuperscriptBox[\\(m\\), \\(*\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\),vec]},t,valC]
]

KinnersleyNullVector["Schwarzschild",vec_String]:=KinnersleyNullVector[ToMetric["Schwarzschild"],vec]
KinnersleyNullVector["Kerr",vec_String]:=KinnersleyNullVector[ToMetric["Kerr"],vec]


Clear[KinnersleyNullTetrad]
KinnersleyNullTetrad[expr_]:=KinnersleyNullVector[expr,#]&/@{"l","n","m","mStar"}


Clear[KinnersleyDerivative]
KinnersleyDerivative[tt_Tensor?MetricQ,op_String]:=
Module[{r,th,t,phi,deriv},

	{t,r,th,phi}=Symbol/@{"t","r","\[Theta]","\[Phi]"};

	(Switch[op,
		"D",
		RawTensorValues@KinnersleyNullVector[tt,"l"],

		"Delta",
		RawTensorValues@KinnersleyNullVector[tt,"n"],

		"delta",
		RawTensorValues@KinnersleyNullVector[tt,"m"],

		"deltaStar",
		RawTensorValues@KinnersleyNullVector[tt,"mStar"],

		___,
		Print["No KinnersleyDerivative = "<>op];
		Print["Options are \"D\", \"Delta\", \"delta\", and \"deltaStar\"."];
		Abort[]

	].{D[#,t],D[#,r],D[#,th],D[#,phi]})&
]
KinnersleyDerivative["Schwarzschild",vec_String]:=KinnersleyDerivative[ToMetric["Schwarzschild"],vec]
KinnersleyDerivative["Kerr",vec_String]:=KinnersleyDerivative[ToMetric["Kerr"],vec]


SpinCoefficient[coeff_String,opts:OptionsPattern[]]:=
Module[{r,a,th,M,val,conj,rules,delta,schw},

	conj=OptionValue["Conjugate"];
	schw=OptionValue["Schwarzschild"];

	{r,th,a,M}=Symbol/@{"r","\[Theta]","a","M"};
	delta=a^2-2 M r+r^2;
	rules=If[schw,{a->0},{}];

	val=
		Switch[coeff,
				"rho",
				-1/(r-I a Cos[th]),

				"beta",
				- SpinCoefficient["rho",Conjugate->True] Cot[th]/(2Sqrt[2]),

				"pi",
				I a SpinCoefficient["rho"]^2 Sin[th]/Sqrt[2],

				"tau",
				-I a SpinCoefficient["rho"]SpinCoefficient["rho",Conjugate->True] Sin[th]/Sqrt[2],

				"mu",
				SpinCoefficient["rho"]^2 SpinCoefficient["rho",Conjugate->True] delta/2,

				"gamma",
				SpinCoefficient["mu"]+SpinCoefficient["rho"]SpinCoefficient["rho",Conjugate->True] (r-M)/2,

				"alpha",
				SpinCoefficient["pi"]-SpinCoefficient["beta",Conjugate->True],

				"sigma"|"epsilon"|"kappa"|"nu"|"lambda",
				0,

				___,
				Print["No SpinCoefficient = ",coeff];
				Print["Possible options are \"alpha\",\"beta\",\"gamma\",\"epsilon\",\"kappa\",\"lambda\",\"mu\",\"nu\",\"pi\",\"rho\",\"sigma\", and \"tau\"."];
				Abort[]

		]/.rules;

	If[conj,Simplify@ComplexExpand@Conjugate@val,val]
]


Clear[FourVelocity]
FourVelocity["KerrGeneric"]:=
Module[{t,r,th,ph,tau,EE,JJ,M,rhoSq,Delta,ut,ur,uth,uph,QQ,a,x1},

	{t,r,th,ph,tau,EE,JJ,M,QQ,a}=Symbol/@{"t","r","\[Theta]","\[Phi]","\[Tau]","\[ScriptCapitalE]","\[ScriptCapitalJ]","M","Q","a"};
	Delta=r[tau]^2-2M r[tau]+a^2;
	rhoSq=r[tau]^2+a^2 Cos[th[tau]]^2;
		
	ut=1/rhoSq (EE((r[tau]^2+a^2)^2/Delta-a^2 Sin[th[tau]]^2)+a JJ(1-(r[tau]^2+a^2)/Delta));
	ur=1/rhoSq Sqrt[(EE(r[tau]^2+a^2)-a JJ)^2-Delta(r[tau]^2+(JJ-a EE)^2+QQ)];
	uth=1/rhoSq Sqrt[QQ-Cot[th[tau]]^2 JJ^2-a^2 Cos[th[tau]]^2 (1-EE^2)];
	uph=1/rhoSq (Csc[th[tau]]^2 JJ+a EE((r[tau]^2+a^2)/Delta-1)-(a^2 JJ)/Delta);
	x1 = ToCurve[{"FourVelocityGenericKerr","x"},ToMetric["Kerr"],{t[tau],r[tau],th[tau],ph[tau]},tau];
	ToTensorOnCurve[ToTensor[{"FourVelocityGenericKerr","u"},ToMetric["Kerr"],{ut,ur,uth,uph}],x1,"ParametrizedValues"->True]
]


FourVelocity["SchwarzschildGeneric"]:=
Module[{t,r,th,ph,tau,EE,JJ,M,x1,ur},

	{t,r,th,ph,tau,EE,JJ,M}=Symbol/@{"t","r","\[Theta]","\[Phi]","\[Tau]","\[ScriptCapitalE]","\[ScriptCapitalJ]","M"};

	x1 = ToCurve[{"FourVelocityGenericSchwarzschild","x"},ToMetric["Schwarzschild"],{t[tau],r[tau],\[Pi]/2,ph[tau]},tau];
	ur = Sqrt[EE^2-(1-(2M)/r[tau])(1+JJ^2/r[tau]^2)];
	ToTensorOnCurve[ToTensor[{"FourVelocityGenericSchwarzschild","u"},ToMetric["Schwarzschild"],{EE/(1-(2 M)/r[tau]),ur,0,JJ/r[tau]^2}],x1,"ParametrizedValues"->True]
]


(*Clear[FourVelocityVector]
FourVelocityVector[tens_Tensor?MetricQ]:=
Module[{r,th,rp,thp,tau,EE,JJ,vals1,ind,temp1,temp2,u1},

	{r,th,rp,thp,tau,EE,JJ,temp1,temp2}=Symbol/@{"r","\[Theta]","rp","\[Theta]p","\[Tau]","\[ScriptCapitalE]","\[ScriptCapitalJ]","temp1","temp2"};
	ind=PossibleIndices[tens][[1]];
	u1=ToTensor[{"FourVelocity"<>TensorName[tens],"u"},tens,{-EE,temp1,temp2,JJ},{-ind}];
	vals1=TensorValues[u1[ind]]/.{r->rp[tau],th->thp[tau]};
	SetTensorValues[u1[ind],{First@vals1,rp'[tau],thp'[tau],Last@vals1}]
]

FourVelocityVector["Schwarzschild"]:=
Module[{t,rp,EE,JJ,M},
	{t,rp,EE,JJ,M}=Symbol/@{"t","rp","\[ScriptCapitalE]","\[ScriptCapitalJ]","M"};
	SetTensorValues[FourVelocityVector[ToMetric["Schwarzschild"]],{EE/(1-(2 M)/rp[t]),(EE rp'[t])/(1-(2 M)/rp[t]),0,JJ/rp[t]^2}]
]

FourVelocityVector["Kerr"]:=
Module[{t,rp,EE,JJ,M,a},
	{t,rp,EE,JJ,M,a}=Symbol/@{"t","rp","\[ScriptCapitalE]","\[ScriptCapitalJ]","M","a"};
	SetTensorValues[FourVelocityVector[ToMetric["Kerr"]],
		{(-a (a EE-JJ)+((a^2+rp[t]^2) (-a JJ+EE (a^2+rp[t]^2)))/(a^2-2 M rp[t]+rp[t]^2))/rp[t]^2,
		((-a (a EE-JJ)+((a^2+rp[t]^2) (-a JJ+EE (a^2+rp[t]^2)))/(a^2-2 M rp[t]+rp[t]^2)) rp'[t])/rp[t]^2,
		0,
		(-a EE+JJ+(a (-a JJ+EE (a^2+rp[t]^2)))/(a^2-2 M rp[t]+rp[t]^2))/rp[t]^2}]
]
*)


Clear[TensorSphericalHarmonic]
TensorSphericalHarmonic[label_String]:=
Module[{Ylm,YAVal,thTemp,phTemp,l,th,ph,A,B,F,G,eps},

	{Ylm,l,th,ph,A,B,F,G}=Symbol/@{"Ylm","l","\[Theta]","\[Phi]","A","B","F","G"};

	YAVal=Simplify[{D[Ylm[thTemp,phTemp],thTemp],D[Ylm[thTemp,phTemp],phTemp]},{\[Pi]>=thTemp>=0,2\[Pi]>=phTemp>=0}]/.{thTemp->th,phTemp->ph};
	eps=LeviCivitaSymbol["TwoSphere"];

	Switch[label,
		"YA",
		ToTensor[{"HarmonicYA","Y"},ToMetric["TwoSphere"],YAVal,{-A}],
		"XA",
		ContractIndices[MergeTensors[-eps[-A,F]TensorSphericalHarmonic["YA"][-F]],{"HarmonicXA","X"}],
		"YAB",
		MergeTensors[CovariantD[TensorSphericalHarmonic["YA"][-B],-A]
					+1/2 l(l+1)Ylm[th,ph]ToMetric["TwoSphere"][-A,-B],{"HarmonicYAB","Y"}],
		"XAB",
		MergeTensors[-(1/2)(eps[-G,F]CovariantD[TensorSphericalHarmonic["YA"][-F],-B]
				+eps[-B,F]CovariantD[TensorSphericalHarmonic["YA"][-F],-G]),{"HarmonicXAB","X"}][-A,-B],
		___,
		Print["No TensorSphericalHarmonic associated with label ", label];
		Print["Options are: ",{"YA","XA","YAB","XAB"}];
	]
]


Clear[M2Amplitude]
M2Amplitude[label_,metric_String:"SchwarzschildM2"]:=
Module[{htt,htr,hrr,ht,hr,jt,jr,a,b,t,r,metricStr},

	metricStr=Switch[metric,"RN"|"ReissnerNordstromM2","ReissnerNordstromM2","Schw"|"SchwarzschildM2","SchwarzschildM2",___,
					Print["Metric ", metric, " is not a valid M2 metric. Options are \"SchwarzschildM2\" (or \"Schw\") or \"ReissnerNordstromM2\" (or \"RN\")"];
					Abort[];
				];
	
	{htt,htr,hrr,ht,hr,jt,jr,a,b,t,r}=Symbol/@{"htt","htr","hrr","ht","hr","jt","jr","a","b","t","r"};

	Switch[label,
		"ja",
		ToTensor[{"ja"<>metricStr,"j"},ToMetric[metricStr],{jt[t,r],jr[t,r]},{-a}],
		"ha",
		ToTensor[{"ha"<>metricStr,"h"},ToMetric[metricStr],{ht[t,r],hr[t,r]},{-a}],
		"hab",
		ToTensor[{"hab"<>metricStr,"h"},ToMetric[metricStr],{{htt[t,r],htr[t,r]},{htr[t,r],hrr[t,r]}},{-a,-b}],
		___,
		Print["No M2Amplitude associated with label ", label];
		Print["Options are: ",{"hab","ja","ha"}];
	Abort[];
	]
]


End[];

EndPackage[];
