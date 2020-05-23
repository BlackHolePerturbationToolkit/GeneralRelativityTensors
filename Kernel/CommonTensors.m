(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`CommonTensors`",
			{"GeneralRelativityTensors`TensorDefinitions`",
			 "GeneralRelativityTensors`TensorDerivatives`",
			 "GeneralRelativityTensors`TensorManipulation`",
			 "GeneralRelativityTensors`Utils`"}];


RiemannTensor::usage="RiemannTensor[m] returns the Riemann Tensor with \
index positions {\"Down\",\"Down\",\"Down\",\"Down\"} computed from the metric Tensor m.";
RicciTensor::usage="RicciTensor[m] returns the Ricci Tensor with index \
positions {\"Down\",\"Down\"} computed from the metric Tensor m.";
RicciScalar::usage="RicciScalar[m] returns the Ricci scalar computed from \
the metric Tensor m.";
EinsteinTensor::usage="EinsteinTensor[m] returns the Einstein Tensor with \
index positions {\"Down\",\"Down\"} computed from the metric Tensor m.";
WeylTensor::usage="WeylTensor[m] returns the Weyl Tensor with index positions \
{\"Down\",\"Down\",\"Down\",\"Down\"} computed from the metric Tensor m.";
CottonTensor::usage="CottonTensor[m] returns the Cotton Tensor with index positions \
{\"Down\",\"Down\",\"Down\",} computed from the metric Tensor m.";
KretschmannScalar::usage="KretschmannScalar[m] returns the \
Kretschmann scalar (Riemann tensor squared) associated with the metric m.";

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
derivative s on the metric m. It is the appropriate Kinnersley null vector contracted with a partial derivative. Values for \
s are \"D\", \"Delta\", \"delta\", or \"deltaStar\".
KinnersleyDerivative[builtIn,s] is equivalent to \
KinnersleyDerivative[ToMetric[builtIn],s], \
where builtIn can be \"Schwarzschild\" or \"Kerr\"."
SpinCoefficient::usage="SpinCoefficient[m,s,conj] returns the Newman-Penrose \
spin coefficient corresponding to the string s (such as \"alpha\") on the metric m. \
The boolean conj can be set True to return the complex conjugate of the coefficient.
SpinCoefficient[m,s] is equivalent to SpinCoefficient[m,s,False],
SpinCoefficient[builtIn,s,conj] is equivalent to \
SpinCoefficient[ToMetric[builtIn],s,conj], where builtIn \
can be \"Schwarzschild\" or \"Kerr\".";

BianchiIdentities::usage="BianchiIdentities[m,n] returns the \
n-th contracted Bianchi identities, where \
n can be 0, 1, or 2. 
BianchiIdentities[m] is equivalent to BianchiIdentities[m,0].";

MaxwellPotential::usage="MaxwellPotential[builtIn] returns the four-vector A on a \
built-in background index position \"Down\" . \
The current choices for builtIn are \"ReissnerNordstrom\" (or \"RN\") and \"KerrNewman\" (or \"KN\").";
FieldStrengthTensor::usage="FieldStrengthTensor[A] returns the field strength tensor associated with the \
electromagnetic vector potential A with index positions {\"Down\",\"Down\"}.
FieldStrengthTensor[builtIn] is equivalent to FieldStrengthTensor[MaxwellPotential[builtIn]]. \
The current choices for builtIn are \"ReissnerNordstrom\" (or \"RN\") and \"KerrNewman\" (or \"KN\").";
MaxwellStressEnergyTensor::usage="MaxwellStressEnergyTensor[F] returns the stress energy tensor associated with the \
electromagnetic field strength tensor F with index positions {\"Up\",\"Up\"}.
MaxwellStressEnergyTensor[builtIn] is equivalent to MaxwellStressEnergyTensor[FieldStrengthTensor[MaxwellPotential[builtIn]]]. \
The current choices for builtIn are \"ReissnerNordstrom\" (or \"RN\") and \"KerrNewman\" (or \"KN\").";


FourVelocityVector::usage="FourVelocityVector[builtIn] returns the four velocity associated with the string builtIn. \
Choices are \"KerrGeneric\" and \"SchwarzschildGeneric\".";

LeviCivitaSymbol::usage="LeviCivitaSymbol[builtIn] returns the Levi-Civita symbol associated with the 
built-in spacetime. The only current choice for builtIn is \"TwoSphere\" (or \"S2\").";
TensorSphericalHarmonic::usage="TensorSphericalHarmonic[builtIn] returns a Martel-Poisson tensor spherical harmonic \
associated with the string builtIn. Choices are \"YA\", \"XA\", \"YAB\", and \"XAB\".";
M2Amplitude::usage="M2Amplitude[builtIn] returns a Martel-Poisson metric perturbation amplitude \
associated with the string builtIn. Choices are \"hab\", \"ha\", and \"ja\".";


Begin["`Private`"];


Options[RiemannTensor]=Options[TraceReverse];
Options[RicciTensor]=Options[TraceReverse];
Options[RicciScalar]=Options[TraceReverse];
Options[EinsteinTensor]=Options[TraceReverse];
Options[WeylTensor]=Options[TraceReverse];
Options[CottonTensor]=Options[TraceReverse];
Options[FieldStrengthTensor]=Options[TraceReverse];
Options[MaxwellStressEnergyTensor]=Options[TraceReverse];
Options[KretschmannScalar]=Options[TraceReverse];
Options[BianchiIdentities]=Options[TraceReverse];
Options[KinnersleyNullTetrad]=Options[KinnersleyNullVector];

DocumentationBuilder`OptionDescriptions["RiemannTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["RicciTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["RicciScalar"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["EinsteinTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["WeylTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["CottonTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["FieldStrengthTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["MaxwellStressEnergyTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["KretschmannScalar"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["BianchiIdentities"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];


reDef@
ToMetric["Minkowski"]:=
Module[{t,x,y,z},	

	{t,x,y,z}=Symbol/@{"t","x","y","z"};

	ToMetric[{"MinkowskiMetric","\[Eta]"},{t,x,y,z},{{-1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},"Greek"]
];
reDef@ToMetric["Mink"]:=ToMetric["Minkowski"];


reDef@
ToMetric["MinkowskiSpherical"]:=
Module[{t,r,\[Theta],\[Phi]},	

	{t,r,\[Theta],\[Phi]}=Symbol/@{"t","r","\[Theta]","\[Phi]"};

	ToMetric[{"MinkowskiSphericalMetric","\[Eta]"},{t,r,\[Theta],\[Phi]},{{-1,0,0,0},{0,1,0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}},"Greek"]
];
reDef@ToMetric["MinkSph"]:=ToMetric["MinkowskiSpherical"];


reDef@
ToMetric["Schwarzschild"]:=
Module[{t,r,\[Theta],\[Phi],M},	

	{t,r,\[Theta],\[Phi],M}=Symbol/@{"t","r","\[Theta]","\[Phi]","M"};
				
	ToMetric[{"SchwarzschildMetric","g"},{t,r,\[Theta],\[Phi]},{{-1+(2 M)/r,0,0,0},{0,1/(1-(2 M)/r),0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}},"Greek"]
];
reDef@ToMetric["Schw"]:=ToMetric["Schwarzschild"]


reDef@
ToMetric["SchwarzschildM2"]:=
Module[{t,r,M},	

	{t,r,M}=Symbol/@{"t","r","M"};

	ToMetric[{"SchwarzschildM2Metric","g"},{t,r},{{-1+(2 M)/r,0},{0,1/(1-(2 M)/r)}},"Latin"]

];
reDef@ToMetric["SchwM2"]:=ToMetric["SchwarzschildM2"];


reDef@
ToMetric["SchwarzschildS2"]:=
Module[{th,ph,r},

	{th,ph,r}=Symbol/@{"\[Theta]","\[Phi]","r"};
	
	ToMetric[{"SchwarzschildS2Metric","g"},{th,ph},r^2 RawTensorValues[ToMetric["TwoSphere"]],"CapitalLatin"]
];
reDef@ToMetric["SchwS2"]:=ToMetric["SchwarzschildS2"];


reDef@
ToMetric["Kerr"]:=
Module[{t,r,\[Theta],\[Phi],M,a},	

	{t,r,\[Theta],\[Phi],M,a}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","a"};
	
	ToMetric[{"KerrMetric","g"},{t,r,\[Theta],\[Phi]},{{(-a^2+2 M r-r^2+a^2 Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2),0,0,-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2))},
							{0,(r^2+a^2 Cos[\[Theta]]^2)/(a^2-2 M r+r^2),0,0},
							{0,0,r^2+a^2 Cos[\[Theta]]^2,0},
							{-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2)),0,0,(Sin[\[Theta]]^2 ((a^2+r^2)^2-a^2 (a^2-2 M r+r^2) Sin[\[Theta]]^2))/(r^2+a^2 Cos[\[Theta]]^2)}},"Greek"]

];


reDef@
ToMetric["TwoSphere"]:=
Module[{th,ph},
	{th,ph}=Symbol/@{"\[Theta]","\[Phi]"};
	ToMetric[{"TwoSphereMetric","\[CapitalOmega]"},{th,ph},{{1,0},{0,Sin[th]^2}},"CapitalLatin"]
];
reDef@ToMetric["S2"]:=ToMetric["TwoSphere"];


reDef@
ToMetric["ReissnerNordstrom"]:=
Module[{t,r,\[Theta],\[Phi],M,Q},	

	{t,r,\[Theta],\[Phi],M,Q}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","Q"};
	
	ToMetric[{"ReissnerNordstromMetric","g"},{t,r,\[Theta],\[Phi]},{{-1+(2 M)/r-Q^2/r^2,0,0,0},{0,1/(1-(2 M)/r+Q^2/r^2),0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}},"Greek"]
];
reDef@ToMetric["RN"]:=ToMetric["ReissnerNordstrom"];


reDef@
ToMetric["ReissnerNordstromM2"]:=
Module[{t,r,M,Q},	

	{t,r,M,Q}=Symbol/@{"t","r","M","Q"};
	
	ToMetric[{"ReissnerNordstromM2Metric","g"},{t,r},{{-1+(2 M)/r-Q^2/r^2,0},{0,1/(1-(2 M)/r+Q^2/r^2)}},"Latin"]
];
reDef@ToMetric["RNM2"]:=ToMetric["ReissnerNordstromM2"]


reDef@ToMetric["ReissnerNordstromS2"]:=SetTensorName[ToMetric["SchwarzschildS2"],{"ReissnerNordstromS2Metric","g"}];
reDef@ToMetric["RNS2"]:=ToMetric["ReissnerNordstromS2"];


reDef@ToMetric["KerrNewman"]:=
Module[{t,r,\[Theta],\[Phi],M,a,rhoSq,capDelta,QQ},	

	{t,r,\[Theta],\[Phi],M,QQ,a}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","Q","a"};
	rhoSq=r^2+a^2 Cos[\[Theta]]^2;
	capDelta=r^2-2M r+a^2+QQ^2;
	
	ToMetric[{"KerrNewmanMetric","g"},{t,r,\[Theta],\[Phi]},{{-((capDelta-a^2 Sin[\[Theta]]^2)/rhoSq),0,0,(a Sin[\[Theta]]^2 (capDelta-r^2-a^2))/rhoSq},
							{0,rhoSq/capDelta,0,0},
							{0,0,rhoSq,0},
							{(a Sin[\[Theta]]^2 (capDelta-r^2-a^2))/rhoSq,0,0,-((Sin[\[Theta]]^2 (a^2 capDelta Sin[\[Theta]]^2-r^4-2r^2 a^2-a^4))/rhoSq)}},"Greek"]
];
reDef@ToMetric["KN"]:=ToMetric["KerrNewman"]


def@
LeviCivitaSymbol["TwoSphere"]:=
Module[{th,ph,A,B},
	{th,ph,A,B}=Symbol/@{"\[Theta]","\[Phi]","A","B"};
	ToTensor[{"LeviCivitaSymbol","\[CurlyEpsilon]"},ToMetric["TwoSphere"],{{0,Sin[th]},{-Sin[th],0}},{-A,-B}]
];
reDef@LeviCivitaSymbol["S2"]:=LeviCivitaSymbol["TwoSphere"]


def@
RiemannTensor[gT_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,xx,vals,name,simpFn,simpFnNest,chr,
		a,b,c,d,rieValueTrue,rieValue,metVal,gvals,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];
	
	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	xx=Coordinates[gT];
	{a,b,c,d}=Take[PossibleIndices[gT],4];
	n=SpacetimeDimensions[gT];
	chr=RawTensorValues@ChristoffelSymbol[gT,"ActWith"->simpFnNest];
	
	name="RiemannTensor"<>TensorName[gT];
	
	gvals=RawTensorValues[gT];
	metVal[c1_,c2_,x1_,x2_]:=metVal[c1,c2,x1,x2]=D[gvals[[c1,c2]],xx[[x1]],xx[[x2]]];
	metVal[c1_,c2_,x1_,x2_]/;c2<c1:=metVal[c2,c1,x1,x2];
	metVal[c1_,c2_,x1_,x2_]/;x2<x1:=metVal[c1,c2,x2,x1];
	metVal[c1_,c2_,x1_,x2_]/;x2<x1&&c2<c1:=metVal[c2,c1,x2,x1];
	rieValueTrue[aa_,bb_,cc_,dd_]:=rieValueTrue[aa,bb,cc,dd]=
		simpFn[1/2 (metVal[aa,dd,bb,cc]-metVal[aa,cc,bb,dd]+metVal[bb,cc,aa,dd]-metVal[bb,dd,aa,cc])
				+ Sum[gvals[[r,s]](chr[[r,bb,cc]]chr[[s,aa,dd]] - chr[[r,bb,dd]]chr[[s,aa,cc]]),{r,1,n},{s,1,n}]];
	
	rieValue[aa_,bb_,cc_,dd_]/;cc>bb&&dd>cc := -rieValue[aa,dd,bb,cc]-rieValue[aa,cc,dd,bb];
	rieValue[aa_,aa_,aa_,aa_]=0;
	rieValue[aa_,aa_,cc_,cc_]=0;
	rieValue[aa_,bb_,aa_,bb_]/;bb>aa:=rieValueTrue[aa,bb,aa,bb];
	rieValue[aa_,bb_,cc_,dd_]/;bb>aa&&dd>cc&&(aa+bb<=cc+dd):=rieValueTrue[aa,bb,cc,dd];
	rieValue[aa_,aa_,cc_,dd_]=0;
	rieValue[aa_,bb_,cc_,cc_]=0;
	
	rieValue[aa_,bb_,cc_,dd_]/;bb<aa:=-rieValue[bb,aa,cc,dd];
	rieValue[aa_,bb_,cc_,dd_]/;dd<cc:=-rieValue[aa,bb,dd,cc];
	rieValue[aa_,bb_,cc_,dd_]/;bb<aa&&dd<cc:=rieValue[bb,aa,dd,cc];
	rieValue[aa_,bb_,aa_,bb_]/;bb<aa:=rieValue[bb,aa,bb,aa];
	rieValue[aa_,bb_,cc_,dd_]/;bb>aa&&dd>cc&&(aa+bb>cc+dd):=rieValue[cc,dd,aa,bb];

		
	vals = If[RawTensorValues[name,{"Down","Down","Down","Down"}]===Undefined,
				Table[rieValue[i,j,k,l],{i,1,n},{j,1,n},{k,1,n},{l,1,n}],
				RawTensorValues[name,{"Down","Down","Down","Down"}]
			];

	ToTensor[KeySort@Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","MetricQ","Indices"}],
		Association["Metric"->gT,
					"MetricQ"->False,
					"Values"->vals,
					"DisplayName"->"R",
					"Name"->name,
					"Indices"->{-a,-b,-c,-d}]]]
]


def@
RicciTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,name,i,j,k,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];


	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	rie=RiemannTensor[g,"ActWithNested"->simpFnNest];
	name="RicciTensor"<>TensorName[g];
	{i,j,k}=Take[PossibleIndices[rie],3];
	
	If[RawTensorValues[name,{"Down","Down"}]===Undefined,
		ContractIndices[rie[i,-j,-i,-k],{name,"R"},"ActWith"->simpFn],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
						Association["Metric"->g,
									"MetricQ"->False,
									"Values"->RawTensorValues[name,{"Down","Down"}],
									"DisplayName"->"R",
									"Name"->name,
									"Indices"->{-i,-j}]]]
	]		
]


def@
RicciScalar[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{ric,i,name,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	ric=RicciTensor[g,"ActWithNested"->simpFnNest];
	name="RicciScalar"<>TensorName[g];
	i=First[PossibleIndices[ric]];
	
	If[RawTensorValues[name,{}]===Undefined,
		ContractIndices[ShiftIndices[ric,{-i,i},"ActWith"->simpFnNest],{name,"R"},"ActWith"->simpFn],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{}],
								"DisplayName"->"R",
								"Name"->name,
								"Indices"->{}]]]
	]		

]


def@
EinsteinTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{ricT,ricS,name,i,j,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];
	
	ricT=RicciTensor[g,"ActWithNested"->simpFnNest];
	ricS=RicciScalar[g,"ActWithNested"->simpFnNest];
	{i,j}=Take[PossibleIndices[ricT],2];
		
	name="EinsteinTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Down","Down"}]===Undefined,
		MergeTensors[ricT[-i,-j]-1/2 ricS g[-i,-j],{name,"G"},"ActWith"->simpFn,"ActWithNested"->simpFnNest],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{"Down","Down"}],
								"DisplayName"->"G",
								"Name"->name,
								"Indices"->{-i,-j}]]]
	]		
]


def@
WeylTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,ricT,ricS,dim,i,k,l,m,name,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	dim = SpacetimeDimensions[g];
	If[dim <= 2, Print["Weyl tensor requires SpacetimeDimensions of at least 3"]; AbortVerbose[]];

	{i,k,l,m}=Take[PossibleIndices[g],4];
	rie=RiemannTensor[g,"ActWithNested"->simpFnNest];
	ricT=RicciTensor[g,"ActWithNested"->simpFnNest];
	ricS=RicciScalar[g,"ActWithNested"->simpFnNest];

	name = "WeylTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Down","Down","Down","Down"}]===Undefined,
		MergeTensors[ShiftIndices[rie,{-i,-k,-l,-m},"ActWith"->simpFnNest]+
				1/(dim-2) (ricT[-i,-m]g[-k,-l]-ricT[-i,-l]g[-k,-m]+ricT[-k,-l]g[-i,-m]-ricT[-k,-m]g[-i,-l])
				+ricS/((dim-1)(dim-2)) (g[-i,-l]g[-k,-m]-g[-i,-m]g[-k,-l]),{name,"C"},"ActWith"->simpFn,"ActWithNested"->simpFnNest],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{"Down","Down","Down","Down"}],
								"DisplayName"->"C",
								"Name"->name,
								"Indices"->{-i,-k,-l,-m}]]]
	]
]


def@
CottonTensor[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{ricT,ricS,dim,i,j,k,name,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	dim = SpacetimeDimensions[g];
	If[dim <= 2, Print["Cotton tensor requires SpacetimeDimensions of at least 3"]; AbortVerbose[]];

	{i,j,k}=Take[PossibleIndices[g],3];
	ricT=RicciTensor[g,"ActWithNested"->simpFnNest];
	ricS=RicciScalar[g,"ActWithNested"->simpFnNest];

	name = "CottonTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Down","Down","Down"}]===Undefined,
		MergeTensors[CovariantD[ricT[-i,-j],-k]-CovariantD[ricT[-i,-k],-j]
			+1/(2(SpacetimeDimensions[g]-1)) (CovariantD[MergeTensors[ricS g[-i,-k],"ActWithNested"->simpFnNest],-j]
								- CovariantD[MergeTensors[ricS g[-i,-j],"ActWithNested"->simpFnNest],-k]),
								{name,"C"},"ActWith"->simpFn,"ActWithNested"->simpFnNest],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{"Down","Down","Down"}],
								"DisplayName"->"C",
								"Name"->name,
								"Indices"->{-i,-j,-k}]]]
	]
]


def@
MaxwellPotential["ReissnerNordstrom"]:=
Module[{QQ,r,ind,met},
	met=ToMetric["ReissnerNordstrom"];
	ind=PossibleIndices[met][[1]];
	{QQ,r}=Symbol/@{"Q","r"};
	ToTensor[{"MaxwellPotential"<>TensorName[met],"A"},met,{QQ/r,0,0,0},{-ind}]
];
reDef@MaxwellPotential["RN"]:=MaxwellPotential["ReissnerNordstrom"];


reDef@
MaxwellPotential["KerrNewman"]:=
Module[{QQ,r,ind,met,rhoSq,a,th,M},
	met=ToMetric["KerrNewman"];
	ind=PossibleIndices[met][[1]];
	{a,th,QQ,r,M}=Symbol/@{"a","\[Theta]","Q","r","M"};
	rhoSq=r^2+a^2 Cos[th]^2;
	ToTensor[{"MaxwellPotential"<>TensorName[met],"A"},met,{(QQ r)/rhoSq,0,0,-((a r QQ Sin[th]^2)/rhoSq )},{-ind}]
];
reDef@MaxwellPotential["KN"]:=MaxwellPotential["KerrNewman"];


def@
FieldStrengthTensor[AA_Tensor,opts:OptionsPattern[]]:=
Module[{g,name,i,j,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	If[Total@Rank[AA]=!=1,Print["Field strength tensor must be derived from a Rank 1 tensor"];AbortVerbose[]];
	If[AbstractQ[AA],Print["Field strength tensor requires a non-abstract potential"];AbortVerbose[]];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	g=Metric[AA];	
	{i,j}=Take[PossibleIndices[g],2];
	name="FieldStrengthTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Down","Down"}]===Undefined,
		MergeTensors[CovariantD[AA[-i],-j]-CovariantD[AA[-j],-i],{name,"F"},"ActWith"->simpFn,"ActWithNested"->simpFnNest],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{"Down","Down"}],
								"DisplayName"->"F",
								"Name"->name,
								"Indices"->{-i,-j}]]]
	]
];
reDef@FieldStrengthTensor[str_String,opts:OptionsPattern[]]:=FieldStrengthTensor[MaxwellPotential[str],opts];


def@
MaxwellStressEnergyTensor[FF_Tensor,opts:OptionsPattern[]]:=
Module[{g,name,i,k,l,m,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	If[Total@Rank[FF]=!=2,Print["Maxwell stress energy tensor must be derived from a Rank 2 tensor"];AbortVerbose[]];
	If[AbstractQ[FF],Print["Maxwell stress energy requires a non-abstract field strength tensor"];AbortVerbose[]];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	g=Metric[FF];	
	{i,k,l,m}=Take[PossibleIndices[g],4];
	name="MaxwellStressEnergyTensor"<>TensorName[g];
	
	If[RawTensorValues[name,{"Up","Up"}]===Undefined,
		MergeTensors[1/(4\[Pi]) (ShiftIndices[FF,{i,-k},"ActWith"->simpFnNest] ShiftIndices[FF,{l,k},"ActWith"->simpFnNest]
								-1/4 ShiftIndices[g,{i,l},"ActWith"->simpFnNest]FF[-k,-m]ShiftIndices[FF,{k,m},"ActWith"->simpFnNest]),
								{name,"T"},"ActWith"->simpFn,"ActWithNested"->simpFnNest],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{"Up","Up"}],
								"DisplayName"->"T",
								"Name"->name,
								"Indices"->{i,k}]]]
	]
];
reDef@MaxwellStressEnergyTensor[str_String,opts:OptionsPattern[]]:=MaxwellStressEnergyTensor[FieldStrengthTensor[MaxwellPotential[str],opts],opts];


def@
KretschmannScalar[g_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,name,i,j,k,l,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	rie=RiemannTensor[g,"ActWithNested"->simpFnNest];
	{i,j,k,l}=Take[PossibleIndices[rie],4];
		
	name="KretschmannScalar"<>TensorName[g];
	
	If[RawTensorValues[name,{}]===Undefined,
		MergeTensors[ShiftIndices[rie,{i,j,k,l},"ActWith"->simpFnNest]ShiftIndices[rie,{-i,-j,-k,-l},"ActWith"->simpFnNest],
				{name,"K"},"ActWith"->simpFn,"ActWithNested"->simpFnNest],
		ToTensor[KeySort@Join[KeyDrop[Association@@g,{"DisplayName","Name","Metric","MetricQ","Indices"}],
					Association["Metric"->g,
								"MetricQ"->False,
								"Values"->RawTensorValues[name,{}],
								"DisplayName"->"K",
								"Name"->name,
								"Indices"->{}]]]
	]		
]


def@
BianchiIdentities[t_Tensor?MetricQ,contractions_:0,opts:OptionsPattern[]]:=
Module[{rie,ric,ein,i,j,k,l,m,simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];

	If[Not[MemberQ[{0,1,2},contractions]],
		Print["Bianchi identities can only be contracted 0, 1, or 2 times"];
		AbortVerbose[]
	];
	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];
	
	{i,j,k,l,m}=Take[PossibleIndices[t],5];
	rie=RiemannTensor[t,"ActWithNested"->simpFnNest];
	ric=RicciTensor[t,"ActWithNested"->simpFnNest];
	ein=EinsteinTensor[t,"ActWithNested"->simpFnNest];

	Switch[contractions,
		0,
		CovariantD[rie[-i,-j,-k,-l],-m]
		+CovariantD[rie[-i,-j,-m,-k],-l]
		+CovariantD[rie[-i,-j,-l,-m],-k],
		1,
		CovariantD[ric[-i,-j],-k]
		-CovariantD[ric[-i,-k],-j]
		+CovariantD[rie[l,-i,-j,-k],-l],
		2,
		CovariantD[ein[-i,-j],i]
	]
]


def@
KinnersleyNullVector[t_Tensor?MetricQ,vec_String]:=
Module[{r,a,th,M,val,delta,sigma,valC,schw,rules},
	
	If[TensorName[t]=!="SchwarzschildMetric" && TensorName[t]=!="KerrMetric",
		Print["KinnersleyNullVector only available for Kerr and Schwarzschild, but metric ", TensorName[t], " used."];
		AbortVerbose[]
	];

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
			AbortVerbose[]
		]/.rules;

	valC=If[vec==="mStar",Simplify@ComplexExpand@Conjugate@#,#]&@val;

	ToTensor[{vec<>"Kinnersley"<>TensorName[t],If[vec==="mStar",\!\(\*
TagBox[
StyleBox["\"\<\\!\\(\\*SuperscriptBox[\\(m\\), \\(*\\)]\\)\>\"",
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\),vec]},t,valC]
];
reDef@KinnersleyNullVector["Schwarzschild",vec_String]:=KinnersleyNullVector[ToMetric["Schwarzschild"],vec];
reDef@KinnersleyNullVector["Kerr",vec_String]:=KinnersleyNullVector[ToMetric["Kerr"],vec];


def@
KinnersleyNullTetrad[expr_]:=KinnersleyNullVector[expr,#]&/@{"l","n","m","mStar"}


def@
KinnersleyDerivative[tt_Tensor?MetricQ,op_String]:=
Module[{r,th,t,phi},

	If[TensorName[tt]=!="SchwarzschildMetric" && TensorName[tt]=!="KerrMetric",
		Print["KinnersleyDerivative only available for Kerr and Schwarzschild, but metric ", TensorName[tt], " used."];
		AbortVerbose[]
	];

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
		AbortVerbose[]

	].{D[#,t],D[#,r],D[#,th],D[#,phi]})&
];
reDef@KinnersleyDerivative["Schwarzschild",vec_String]:=KinnersleyDerivative[ToMetric["Schwarzschild"],vec];
reDef@KinnersleyDerivative["Kerr",vec_String]:=KinnersleyDerivative[ToMetric["Kerr"],vec];


reDef@
SpinCoefficient[tt_Tensor?MetricQ,coeff_String,conj_?BooleanQ]:=
Module[{r,a,th,M,val,rules,delta,schw},

	If[TensorName[tt]=!="SchwarzschildMetric" && TensorName[tt]=!="KerrMetric",
		Print["SpinCoefficient only available for Kerr and Schwarzschild, but metric ", TensorName[tt], " used."];
		AbortVerbose[]
	];
	schw=TensorName[tt]==="SchwarzschildMetric";

	{r,th,a,M}=Symbol/@{"r","\[Theta]","a","M"};
	delta=a^2-2 M r+r^2;
	rules=If[schw,{a->0},{}];

	val=
		Switch[coeff,
				"rho",
				-1/(r-I a Cos[th]),

				"beta",
				- SpinCoefficient[tt,"rho",True] Cot[th]/(2Sqrt[2]),

				"pi",
				I a SpinCoefficient[tt,"rho"]^2 Sin[th]/Sqrt[2],

				"tau",
				-I a SpinCoefficient[tt,"rho"]SpinCoefficient[tt,"rho",True] Sin[th]/Sqrt[2],

				"mu",
				SpinCoefficient[tt,"rho"]^2 SpinCoefficient[tt,"rho",True] delta/2,

				"gamma",
				SpinCoefficient[tt,"mu"]+SpinCoefficient[tt,"rho"]SpinCoefficient[tt,"rho",True] (r-M)/2,

				"alpha",
				SpinCoefficient[tt,"pi"]-SpinCoefficient[tt,"beta",True],

				"sigma"|"epsilon"|"kappa"|"nu"|"lambda",
				0,

				___,
				Print["No SpinCoefficient = ",coeff];
				Print["Possible options are \"alpha\",\"beta\",\"gamma\",\"epsilon\",\"kappa\",\"lambda\",\"mu\",\"nu\",\"pi\",\"rho\",\"sigma\", and \"tau\"."];
				AbortVerbose[]

		]/.rules;

	If[conj,Simplify@ComplexExpand@Conjugate@val,val]
];
reDef@SpinCoefficient[tt_Tensor?MetricQ,coeff_String]:=SpinCoefficient[tt,coeff,False];
reDef@SpinCoefficient[str_String,coeff_String,conj_?BooleanQ]:=SpinCoefficient[ToMetric[str],coeff,conj];
reDef@SpinCoefficient[str_String,coeff_String]:=SpinCoefficient[str,coeff,False];


def@
FourVelocityVector["KerrGeneric"]:=
Module[{t,r,th,ph,tau,EE,JJ,M,rhoSq,Delta,ut,ur,uth,uph,QQ,a,x1},

	{t,r,th,ph,tau,EE,JJ,M,QQ,a}=Symbol/@{"t","r","\[Theta]","\[Phi]","\[Tau]","\[ScriptCapitalE]","\[ScriptCapitalL]","M","Q","a"};
	Delta=r[tau]^2-2M r[tau]+a^2;
	rhoSq=r[tau]^2+a^2 Cos[th[tau]]^2;
		
	ut=1/rhoSq (EE((r[tau]^2+a^2)^2/Delta-a^2 Sin[th[tau]]^2)+a JJ(1-(r[tau]^2+a^2)/Delta));
	ur=1/rhoSq Sqrt[(EE(r[tau]^2+a^2)-a JJ)^2-Delta(r[tau]^2+(JJ-a EE)^2+QQ)];
	uth=1/rhoSq Sqrt[QQ-Cot[th[tau]]^2 JJ^2-a^2 Cos[th[tau]]^2 (1-EE^2)];
	uph=1/rhoSq (Csc[th[tau]]^2 JJ+a EE((r[tau]^2+a^2)/Delta-1)-(a^2 JJ)/Delta);
	x1 = ToCurve[{"CurveGenericKerr","x"},ToMetric["Kerr"],{t[tau],r[tau],th[tau],ph[tau]},tau];
	
	ToTensorOnCurve[{"FourVelocityVectorGenericKerr","u"},x1,{ut,ur,uth,uph}]
];
reDef@FourVelocityVector["KerrGen"]:=FourVelocityVector["KerrGeneric"];


reDef@
FourVelocityVector["SchwarzschildGeneric"]:=
Module[{t,r,th,ph,tau,EE,JJ,M,x1,ur},

	{t,r,th,ph,tau,EE,JJ,M}=Symbol/@{"t","r","\[Theta]","\[Phi]","\[Tau]","\[ScriptCapitalE]","\[ScriptCapitalL]","M"};

	x1 = ToCurve[{"CurveGenericSchwarzschild","x"},ToMetric["Schwarzschild"],{t[tau],r[tau],\[Pi]/2,ph[tau]},tau];
	ur = Sqrt[EE^2-(1-(2M)/r[tau])(1+JJ^2/r[tau]^2)];

	ToTensorOnCurve[{"FourVelocityVectorGenericSchwarzschild","u"},x1,{EE/(1-(2 M)/r[tau]),ur,0,JJ/r[tau]^2}]
];
reDef@FourVelocityVector["SchwGen"]:=FourVelocityVector["SchwarzschildGeneric"];


def@
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


def@
M2Amplitude[label_,metric_String:"SchwarzschildM2"]:=
Module[{htt,htr,hrr,ht,hr,jt,jr,a,b,t,r,metricStr},

	metricStr=Switch[metric,"RN"|"ReissnerNordstromM2","ReissnerNordstromM2","Schw"|"SchwarzschildM2","SchwarzschildM2",___,
					Print["Metric ", metric, " is not a valid M2 metric. Options are \"SchwarzschildM2\" (or \"Schw\") or \"ReissnerNordstromM2\" (or \"RN\")"];
					AbortVerbose[];
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
		AbortVerbose[];
	]
]


End[];

EndPackage[];
