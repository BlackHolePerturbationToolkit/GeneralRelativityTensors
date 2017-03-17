(* ::Package:: *)

BeginPackage["Tensors`"];


Tensor::usage="Tensor is a Head created with the command ToTensor.";
ToTensor::usage="ToTensor[n,{inds}] returns a tensor with TensorName n and indices {inds}.
ToTensor[n,m,vals] returns a contravariant tensor with TensorName n. The (non-Abstract) metric m and values vals (given as a consistently sized List) are assigned.";
(*ToMetric::usage="ToMetric[n,coords,inds] returns an Abtract metric Tensor with TensorName n, Coordinates coords and Indices inds.
ToMetric[n,coords] is equivalent to ToMetric[n,coords,\"Greek\"].
ToMetric[n,coords,vals,inds] returns an non-Abtract metric Tensor with TensorName n, Coordinates coords, TensorValues vals, and PossibleIndices inds \
(where inds can be \"Greek\",\"Latin\",\"CaptialLatin\" or a list of Symbols).
ToMetric[n,coords,vals] is equivalent to ToMetric[n,coords,vals \"Greek\"].
ToMetric[builtIn] returns a built-in metric Tensor, where builtIn can be \"Minkowski\", \"Schwarzschild\", or \"Kerr\".";*)
ToMetric::usage="ToMetric[n,coords,vals,inds] returns an non-Abtract metric Tensor with TensorName n, Coordinates coords, TensorValues vals, and PossibleIndices inds \
(where inds can be \"Greek\",\"Latin\",\"CaptialLatin\" or a list of Symbols).
ToMetric[builtIn] returns a built-in metric Tensor, where builtIn can be \"Minkowski\", \"Schwarzschild\", or \"Kerr\".";
Coordinates::usage="Coordinates[t] returns a List of symbols used for the coordinates of the Tensor t, or Undefined if coordinates were not set.";
Metric::usage="Metric[t] returns the metric tensor associated with the Tensor t, or Undefined if no metric was set. Note that t will return itself if it is a metric.";
InverseMetric::usage="InverseMetric[t] returns the inverse metric tensor associated with the Tensor t, or Undefined if no metric was set.";
Rank::usage="Rank[t] returns the tensor rank of the Tensor t as a List {p,q}, where p is the number of contravariant indices and q the number of covariant indices.";
Indices::usage="Indices[t] returns a List of Symbols representing the indices of the Tensor t. Positive Symbols are contravariant and negative Symbols are covariant.";
PossibleIndices::usage="PossibleIndices[t] returns a List of all possible Symbols that can represent the indices of the Tensor t.";
TensorName::usage="TensorName[t] returns the name of Tensor t which is used for storing cached values in the Symbol TensorValues.";
TensorDisplayName::usage="TensorDisplayName[t] returns the name of Tensor t that is used for formatted output.";
IndexPositions::usage="IndexPositions[t] returns a List of elements \"Up\" and \"Down\" which represent (respectively) the contravariant and covariant positions of the indices of Tensor t.";
ChristoffelSymbol::usage="ChristoffelSymbol[m] returns the Christoffel symbol computed from the metric tensor m.";
RiemannTensor::usage="RiemannTensor[m] returns the Riemann tensor with indices {\"Up\",\"Down\",\"Down\",\"Down\"} computed from the metric tensor m.";
RicciTensor::usage="RicciTensor[m] returns the Ricci tensor with indices {\"Down\",\"Down\"} computed from the metric tensor m.";
RicciScalar::usage="RicciScalar[m] returns the Ricci scalar computed from the metric tensor m.";
ContractIndices::usage="ContractIndices[t,n] contracts all repeated indices of Tensor t, returning the resulting lower-rank tensor with name n.
ContractIndices[t] is equivalent to ContractIndices[t,{TensorName[t],TensorDisplayName[t]}].";
ShiftIndices::usage="ShiftIndices[t,inds] raises and/or lowers the indices of Tensor t according to the given List inds, adjusting the values using the tensor's associated metric.";
ValidateIndices::usage="ValidateIndices[t,{inds}] checks that the list of indices {inds} is valid for Tensor t. An error is printed an operation is aborted if the list is not valid.";
TensorValues::usage="TensorValues[n,{inds}] returns the cached values of a Tensor with TensorName n and indices in positions {inds} or Undefined if none have been computed. The List {inds} should contain elements \"Up\" and/or \"Down\".
TensorValues[t] is equivalent to TensorValues[TensorName[t],IndexPositions[t]].";
RenameTensor::usage="RenameTensor[t,n] returns the Tensor t with its TensorName changed to n.";
MergeTensors::usage="MergeTensors[expr,n] calls MultiplyTensors, MultiplyTensorScalar, and SumTensors to merge the tensor expression expr into one Tensor with TensorName n.
MergeTensors[expr] merges the tensor expression expr and forms a new TensorName and TensorDisplayName from a combination of the tensors making up the expression.";
SumTensors::usage="SumTensors[t1,t2,..,n] sums the Tensors t1, t2, etc., forming a new Tensor with TensorName n.
SumTensors[t1,t2,..] sums the Tensors t1, t2, etc., and forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
MultiplyTensors::usage="MultiplyTensors[t1,t2,..,n] forms the outer product of the Tensors t1, t2, etc., creating a new Tensor with TensorName n.
MultiplyTensors[t1,t2,..] forms the outer product of the Tensors t1, t2, etc., and forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
MultiplyTensorScalar::usage="MultiplyTensorScalar[a, t, n] or MultiplyTensorScalar[t, a, n] forms the product of the scalar a with the Tensor t, creating a new Tensor with TensorName n.
MultiplyTensorScalar[a, t] forms the product of the a and t, and forms a new TensorName and TensorDisplayName from a combination of the scalar and Tensor making up the expression.";
RepeatedIndexQ::usage="RepeatedIndexQ[t] returns True if the Tensor t has repeated indices which can be traced.";
MetricQ::usage="MetricQ[t] returns True if the Tensor t is a metric.";
AbstractQ::usage="AbstractQ[t] returns True if the Tensor t is treated as Abstract.";
ClearCachedTensorValues::usage="ClearCachedTensorValues[n,inds] removes cached expressions stored with the Symbol TensorValues using the TensorName n and IndexPositions inds. Here inds is a List of \"Up\" and \"Down\".
ClearCachedTensorValues[t] removes all cached expressions stored with the Symbol TensorValues for the Tensor t.
ClearCachedTensorValues[All] removes all cached expressions associated with the Symbol TensorValues.";
CachedTensorValues::usage="CachedTensorValues[n] returns a List of Rules showing all cached expressions for the TensorName n (stored in the Symbol TensorValues).
CachedTensorValues[t] returns a List of Rules showing all cached expressions for the Tensor t (stored in the Symbol TensorValues).
CachedTensorValues[All] returns a List of Rules showing all cached expressions (stored in the Symbol TensorValues)."
Component::usage="Component[t,inds] returns the component of Tensor t with (appropriately covariant and contravariant) indices inds. All elements of inds must be Coordinates of t.";
KinnersleyNullVector::usage="KinnersleyNullVector[m,v] returns the contravariant Kinnersley null vector associated with metric tensor m and string v, where v can be \"l\", \"n\", \"m\", or \"mStar\".
KinnersleyNullVector[builtIn,v] is equivalent to KinnersleyNullVector[ToMetric[builtIn],v], where builtIn can be \"Schwarzschild\" or \"Kerr\"."
KinnersleyNullTetrad::uusage="KinnersleyNullTetrad[m] returns a list of the four KinnersleyNullVector in order {\"l\", \"n\", \"m\", \"mStar\"} for the metric m.
KinnersleyNullTetrad[builtIn] is equivalent to KinnersleyNullTetrad[ToMetric[builtIn]], where builtIn can be \"Schwarzschild\" or \"Kerr\"."
KinnersleyDerivative::usage="KinnersleyDerivative[m,s] returns the projected derivative s being the appropriate Kinnersley null vector contracted with a partial derivative. Values for s are \"D\", \"Delta\", \"delta\", or \"deltaStar\".
KinnersleyDerivative[builtIn,s] is equivalent to KinnersleyDerivative[ToMetric[builtIn],s], where builtIn can be \"Schwarzschild\" or \"Kerr\"."
SpinCoefficient::usage="SpinCoefficient[s] returns the Newman-Penrose spin coefficient corresponding to the string s, where possible values of s are \
\"alpha\",\"beta\",\"gamma\",\"epsilon\",\"kappa\",\"lambda\",\"mu\",\"nu\",\"pi\",\"rho\",\"sigma\", and \"tau\".";
CovariantD;
FourVelocity;
LeviCivitaSymbol;
TensorHarmonic;
ActOnTensorValues;
SetTensorValues;


Begin["`Private`"];


Tensor/:Format[t_Tensor]:=formatTensor[TensorDisplayName@t,Indices@t]


Clear[formatTensor]
formatTensor[name_,inds_]:=
Module[{upStr,dnStr},
	If[inds==={},
		name,
		dnStr=StringJoin[If[MatchQ[#,-_Symbol],ToString[#/.-x_:>x],"  "]&/@inds];
		upStr=StringJoin[If[Not@MatchQ[#,-_Symbol],ToString[#],"  "]&/@inds];
		DisplayForm@SubsuperscriptBox[name,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
	]
]


Tensor/:Coordinates[t_Tensor]:=(Association@@t)["Coordinates"]
Tensor/:Metric[t_Tensor]:=If[(Association@@t)["Metric"]==="Self",t,(Association@@t)["Metric"]];
Tensor/:Rank[t_Tensor]:=Module[{inds,co},inds=Indices[t];co=Count[inds,-_Symbol];{Length[inds]-co,co}];
Tensor/:AbstractQ[t_Tensor]:=(Association@@t)["Abstract"]
Tensor/:Dimensions[t_Tensor]:=(Association@@t)["Dimensions"]
Tensor/:Indices[t_Tensor]:=(Association@@t)["Indices"]
Tensor/:PossibleIndices[t_Tensor]:=(Association@@t)["PossibleIndices"]
Tensor/:TensorName[t_Tensor]:=(Association@@t)["Name"]
Tensor/:TensorDisplayName[t_Tensor]:=(Association@@t)["DisplayName"]
Tensor/:IndexPositions[t_Tensor]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[t];
Tensor/:RepeatedIndexQ[t_Tensor]:=Length[DeleteDuplicates@(Indices[t]/.-sym_:>sym)]<Length[Indices[t]];
Tensor/:MetricQ[t_Tensor]:=(Association@@t)["IsMetric"]
Tensor/:t_Tensor[inds__]/;Complement[{inds}/.-sym_:>sym,PossibleIndices[t]]==={}:=ShiftIndices[t,{inds}]
Tensor/:t_Tensor[inds__]/;Complement[{inds}/.-sym_:>sym,Coordinates[t]]==={}:=Component[t,{inds}]
Tensor/:t_Tensor[inds__]:=(Print["The given indices ",{inds}, " are neither entirely in the List of PossibleIndices, nor Coordinates of ", t];Abort[])


TensorValues[___]:=Undefined;
TensorValues[t_Tensor]:=If[#=!=Undefined,If[AutoNameQ[t],#,TensorValues[TensorName[t],IndexPositions[t]]=#],Undefined]&[(Association@@t)["Values"]];


Clear[ToTensor]
ToTensor[assoc_Association]:=
Module[{keys,nullKeys,listKeys,indexChoices},
	keys={"IsMetric","Metric","Coordinates","Name","DisplayName","Indices","Values","Abstract","Dimensions","PossibleIndices"};
	nullKeys={"Metric","Coordinates","Values","PossibleIndices","Dimensions"};
	listKeys={"Coordinates","PossibleIndices","Indices"};

	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the tensor formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the tensor formation: "<>ToString[Complement[Keys[assoc],keys]]];
		Abort[]
	];
	If[Not@MatchQ[assoc["Indices"]/.-sym_:>sym,{___Symbol}],Print["Indices must be a list of Symbols (and negative Symbols)"];Abort[]];
	If[Not@MatchQ[assoc["PossibleIndices"],{___Symbol}],Print["PossibleIndices must be a list of Symbols"];Abort[]];

	indexChoices=Union@Join[assoc["PossibleIndices"],assoc["Indices"]/.-sym_:>sym];

	If[DeleteDuplicates[If[assoc[#]=!=Undefined,Head[assoc[#]],##&[]]&/@listKeys]=!={List},
		Print["The following Options were not given as lists: "<>ToString[If[assoc[#]=!=Undefined&&Head[assoc[#]]=!=List,#,##&[]]&/@listKeys]];
		Abort[]
	];

	If[Not[assoc["Abstract"]]&&MemberQ[assoc[#]&/@nullKeys,Undefined],
		Print["\"Abstract\"->False is inconsistent with Undefined values for "<>ToString[If[assoc[#]===Undefined,#,##&[]]&/@nullKeys]];
		Abort[]
	];
	
	If[(assoc["Coordinates"]=!=Undefined&&assoc["Dimensions"]=!=Undefined)&&Length@assoc["Coordinates"]=!=assoc["Dimensions"],
		Print["The number of coordinates given does not match the number of dimensions."];
		Abort[]
	];

	If[(MatchQ[assoc["Values"],_List]&&assoc["Values"]=!=Undefined&&assoc["Dimensions"]=!=Undefined)&&
							Dimensions[assoc["Values"]]=!=Table[assoc["Dimensions"],{Length[assoc["Indices"]]}],
		Print["Provided values are inconsistent with given tensor rank and number of dimensions."];
		Abort[]
	];

	If[Not@MatchQ[assoc["Values"],_List]&&assoc["Values"]=!=Undefined&&Length[assoc["Indices"]]=!=0,
		Print["Scalar quantity given with indices."];
		Abort[]
	];

	If[assoc["Metric"]=!=Undefined&&assoc["Metric"]=!="Self"&&Not@MetricQ[assoc["Metric"]],
		Print["Given Option \"Metric\" is not a metric tensor."];
		Abort[]
	];
	
	If[assoc["Coordinates"]=!=Undefined&&Intersection[assoc["Coordinates"],indexChoices]=!={},
		Print["The following elements appear as both indices and coordinates: "<>ToString[Intersection[assoc["Coordinates"],indexChoices]]];
		Abort[]
	];
		
	If[DeleteDuplicates[If[assoc[#]=!=Undefined,Head[assoc[#]],##&[]]&/@listKeys]=!={List},
		Print[DeleteDuplicates[If[assoc[#]=!=Undefined,Head[assoc[#]],##&[]]&/@listKeys]];
		Print["The following Options must be given as lists: "<>ToString[If[assoc[#]=!=Undefined&&Head[assoc[#]]=!=List,#,##&[]]&/@listKeys]];
		Abort[]
	];
		
	If[Not@BooleanQ[assoc["IsMetric"]],
		Print["\"IsMetric\" must be True or False."];
		Abort[]
	];

	If[#=!=Undefined&&Not[AutoNameQ[assoc["Name"]]],TensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];
	Tensor@@(Normal@assoc/.("PossibleIndices"->_):>("PossibleIndices"->indexChoices))
]


Options[ToTensor]={"Coordinates"->Undefined,"DisplayName"->Undefined,"Metric"->Undefined,"IsMetric"->False,"PossibleIndices"->{},"Abstract"->True,"Values"->Undefined,"Dimensions"->Undefined};
ToTensor[{name_String,dispName_String},{inds___},opts:OptionsPattern[]]:=
Module[{coords,vals,posInds,abstr,metric,dims,isMetric},
	coords=OptionValue["Coordinates"];
	vals=OptionValue["Values"];
	posInds=OptionValue["PossibleIndices"];
	abstr=OptionValue["Abstract"];
	metric=OptionValue["Metric"];
	isMetric=OptionValue["IsMetric"];
	dims=OptionValue["Dimensions"];
	ToTensor[Association["Coordinates"->coords,"Metric"->metric,"IsMetric"->isMetric,"Name"->name,"DisplayName"->dispName,"Indices"->{inds},"PossibleIndices"->posInds,"Abstract"->abstr,"Values"->vals,"Dimensions"->dims]]
]
ToTensor[name_String,{inds___},opts:OptionsPattern[]]:=ToTensor[{name,name},{inds},opts]


ToTensor[{name_String,dispName_String},metric_Tensor?MetricQ,vals_List,indsGiven_:Undefined]:=
Module[{coords,posInds,dims,inds},

	If[AbstractQ[metric],Print["Tensor with values cannot be defined using \"Abstract\" metric."];Abort[]];

	coords=Coordinates[metric];	
	posInds=PossibleIndices[metric];
	dims=Dimensions[metric];
	inds=If[indsGiven===Undefined,Take[posInds,Length@Dimensions[vals]],indsGiven];
	ToTensor[Association["Coordinates"->coords,"Metric"->metric,"IsMetric"->False,"Name"->name,"DisplayName"->dispName,"Indices"->inds,"PossibleIndices"->posInds,"Abstract"->False,"Values"->vals,"Dimensions"->dims]]
]
ToTensor[name_String,metric_Tensor?MetricQ,vals_List]:=ToTensor[{name,name},metric,vals];


Clear[builtInIndices]
builtInIndices[label_]:=
Switch[label,
		"Latin",
		Symbol/@CharacterRange["a","z"],
		"CapitalLatin",
		Symbol/@Complement[CharacterRange["A","Z"],{"D","C","E","I","K","N","O"}],
		"Greek",
		Symbol/@Complement[CharacterRange["\[Alpha]","\[Omega]"],{"\[Pi]"}],
		___,
		Print["No built-in indices ", label]; Abort[]
]


Clear[ToMetric]
ToMetric[assoc_Association]:=
Module[{keys,dims,posInds,inds},
	
	keys={"Coordinates","Name","Indices","Values","Abstract","PossibleIndices","DisplayName"};

	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the metric tensor formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the metric tensor formation: "<>ToString[Complement[Keys[assoc],keys]]];
		Abort[]
	];
	posInds=Complement[If[MemberQ[{"Greek","Latin","CapitalLatin"},assoc["PossibleIndices"]],builtInIndices[assoc["PossibleIndices"]],assoc["PossibleIndices"]],
						Union[If[assoc["Coordinates"]=!=Undefined,assoc["Coordinates"],##&[]],Cases[assoc["Values"],_Symbol,Infinity]]];
	inds=If[assoc["Indices"]===Undefined,-Take[posInds,2],assoc["Indices"]];

	If[Not@MatchQ[inds,{-_Symbol,-_Symbol}]||(inds[[1]]===inds[[2]]),Print["Metric indices must be a pair of distinct covariant symbols"];Abort[]];
	If[assoc["Values"]=!=Undefined&&(Not@MatchQ[assoc["Values"],{Repeated[{__}]}]||Dimensions[assoc["Values"]]=!={Length@assoc["Coordinates"],Length@assoc["Coordinates"]}),
		Print["To be consistent with given coordinates, metric values must be given as a ",Length@assoc["Coordinates"], " \[Times] ", Length@assoc["Coordinates"], " matrix."];
		Abort[]
	];

	dims=If[assoc["Coordinates"]=!=Undefined,Length@assoc["Coordinates"],assoc["Coordinates"]];
	ToTensor[Join[KeyDrop[assoc,{"PossibleIndices","Indices"}],Association["Metric"->"Self","IsMetric"->True,"Dimensions"->dims,"PossibleIndices"->posInds,"Indices"->inds]]]
]


(*ToMetric[{name_String,dispName_String},coords_,posIndsParam_:"Greek"]:=
Module[{inds,posInds},
	posInds=Complement[
					If[MemberQ[{"Greek","Latin","CapitalLatin"},posIndsParam],builtInIndices[posIndsParam],posIndsParam],
					If[coords=!=Undefined,coords,{}]
			];
	inds=-Take[posInds,2];
	ToMetric[Association["Coordinates"->coords,"Name"->name,"DisplayName"->dispName,"Indices"->inds,"PossibleIndices"->posInds,"Abstract"->True,"Values"->Undefined]]
]
ToMetric[name_String]:=ToMetric[{name,name}]*)


ToMetric[{name_String,dispName_String},coords_List,vals_List,posIndsParam_]:=
Module[{inds,posInds},

	posInds=Complement[
					If[MemberQ[{"Greek","Latin","CapitalLatin"},posIndsParam],builtInIndices[posIndsParam],posIndsParam],
					Union[coords,Cases[vals,_Symbol,Infinity]]
			];

	inds=-Take[posInds,2];
	
	ToMetric[
		Association["Coordinates"->coords,
					"Name"->name,
					"DisplayName"->dispName,
					"Indices"->inds,
					"PossibleIndices"->posInds,
					"Abstract"->False,
					"Values"->vals]
		]
]
ToMetric[{name_String,dispName_String},coords_,vals_]:=ToMetric[{name,dispName},coords,vals,"Greek"]
ToMetric[name_String,coords_,vals_,posIndsParam_]:=ToMetric[{name,name},coords,vals,posIndsParam]
ToMetric[name_String,coords_,vals_]:=ToMetric[{name,name},coords,vals,"Greek"]


ToMetric["Minkowski"]:=
Module[{t,x,y,z,\[Alpha],\[Beta]},	

	{t,x,y,z,\[Alpha],\[Beta]}=Symbol/@{"t","x","y","z","\[Alpha]","\[Beta]"};

	ToMetric[Association["Name"->"MinkowskiMetric",
				"Coordinates"->{t,x,y,z},
				"DisplayName"->"\[Eta]",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]]
]


ToMetric["Schwarzschild"]:=
Module[{t,r,\[Theta],\[Phi],M,\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],M,\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","\[Alpha]","\[Beta]"};
	
	ToMetric[Association["Name"->"SchwarzschildMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"g",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1+(2 M)/r,0,0,0},{0,1/(1-(2 M)/r),0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}}]]
]


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
							{-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2)),0,0,(Sin[\[Theta]]^2 ((a^2+r^2)^2-a^2 (a^2-2 M r+r^2) Sin[\[Theta]]^2))/(r^2+a^2 Cos[\[Theta]]^2)}}]]
]


ToMetric["TwoSphere"]:=
Module[{th,ph},
	{th,ph}=Symbol/@{"\[Theta]","\[Phi]"};
	ToMetric[{"TwoSphereMetric","\[CapitalOmega]"},{th,ph},{{1,0},{0,Sin[th]^2}},"CapitalLatin"]
]


Clear[LeviCivitaSymbol]
LeviCivitaSymbol["TwoSphere"]:=
Module[{th,ph,A,B},
	{th,ph,A,B}=Symbol/@{"\[Theta]","\[Phi]","A","B"};
	ToTensor[{"LeviCivitaSymbol","\[CurlyEpsilon]"},ToMetric["TwoSphere"],{{0,Sin[th]},{-Sin[th],0}},{-A,-B}]
]


Clear[InverseMetric]
Tensor/:InverseMetric[t_Tensor]:=If[Metric@t===Undefined,Undefined,InverseMetric@Metric@t];
Tensor/:InverseMetric[t_Tensor?MetricQ]:=InverseMetric[t]=
Module[{assoc,tvStored,tv,posUp},

	posUp={"Up","Up"};
	tvStored=TensorValues[TensorName[t],posUp];
	tv=If[tvStored===Undefined,
			If[TensorValues[t]===Undefined,
				Undefined,
				Simplify@Inverse[TensorValues[Metric[t]]]
			],
			tvStored
		];
	
	assoc=Association@@t;
	ToTensor[Join[KeyDrop[assoc,{"Indices","Metric"}],Association["Indices"->Indices[t]/.-sym_Symbol:>sym,"Values"->tv,"Metric"->Metric[t]]]]
]


Options[ChristoffelSymbol]={"SimplifyFunction"->Identity};
Tensor/:ChristoffelSymbol[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,g,ig,xx,vals,posInds,gT,name,simpFn},
	simpFn=OptionValue["SimplifyFunction"];
	gT=Metric[t];
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	g=TensorValues[gT];
	ig=TensorValues@InverseMetric[gT];
	name="ChristoffelSymbol"<>TensorName[t];
	vals=
		If[TensorValues[name,{"Up","Down","Down"}]===Undefined,
			simpFn@Table[(1/2)Sum[ig[[i,s]](-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}],
			TensorValues[name,{"Up","Down","Down"}]
		];

	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","IsMetric","Indices"}],
			Association["Metric"->gT,"IsMetric"->False,"Values"->vals,"DisplayName"->"\[CapitalGamma]","Name"->name,"Indices"->{posInds[[1]],-posInds[[2]],-posInds[[3]]}]]]
]


Options[RiemannTensor]=Options[ChristoffelSymbol];
Tensor/:RiemannTensor[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,g,ig,xx,chr,vals,posInds,gT,name,simpFn},
	simpFn=OptionValue["SimplifyFunction"];
	gT=Metric[t];
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	g=TensorValues[gT];
	ig=TensorValues@InverseMetric[gT];
	chr=TensorValues@ChristoffelSymbol[gT,"SimplifyFunction"->simpFn];
	name="RiemannTensor"<>TensorName[t];
	vals=
		If[TensorValues[name,{"Up","Down","Down","Down"}]===Undefined,
			simpFn@Table[D[chr[[i,k,m]],xx[[l]]]-D[chr[[i,k,l]],xx[[m]]]
						+Sum[chr[[i,s,l]]chr[[s,k,m]],{s,1,n}]
						-Sum[chr[[i,s,m]]chr[[s,k,l]],{s,1,n}],
							{i,1,n},{k,1,n},{l,1,n},{m,1,n}],
			TensorValues[name,{"Up","Down","Down","Down"}]
		];

	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","IsMetric","Indices"}],
		Association["Metric"->gT,"IsMetric"->False,"Values"->vals,"DisplayName"->"R","Name"->name,"Indices"->{posInds[[1]],-posInds[[2]],-posInds[[3]],-posInds[[4]]}]]]
]


Options[RicciTensor]=Options[ChristoffelSymbol];
Tensor/:RicciTensor[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{rie,inds,simpFn},
	simpFn=OptionValue["SimplifyFunction"];
	rie=RiemannTensor[t,"SimplifyFunction"->simpFn];
	inds=Indices[rie];
	ActOnTensorValues[ContractIndices[rie[inds[[1]],inds[[2]],-inds[[1]],inds[[4]]],{"RicciTensor"<>TensorName[t],"R"}],simpFn]
]


Options[RicciScalar]=Options[ChristoffelSymbol];
Tensor/:RicciScalar[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{ric,inds,simpFn},

	simpFn=OptionValue["SimplifyFunction"];
	ric=RicciTensor[t,"SimplifyFunction"->simpFn];
	inds=Indices[ric];
	ActOnTensorValues[ContractIndices[ric[-inds[[1]],inds[[1]]],{"RicciScalar"<>TensorName[t],"R"}],simpFn]
]


Options[KinnersleyNullVector]={"Schwarzschild"->False};
Clear[KinnersleyNullVector]
KinnersleyNullVector[t_Tensor?MetricQ,vec_String,opts:OptionsPattern[]]:=
Module[{r,a,th,M,val,delta,sigma,valC,schw,rules},
	schw=OptionValue["Schwarzschild"];

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

KinnersleyNullVector["Schwarzschild",vec_String,opts:OptionsPattern[]]:=KinnersleyNullVector[ToMetric["Schwarzschild"],vec,"Schwarzschild"->True]
KinnersleyNullVector["Kerr",vec_String,opts:OptionsPattern[]]:=KinnersleyNullVector[ToMetric["Kerr"],vec,"Schwarzschild"->False]


Options[KinnersleyNullTetrad]=Options[KinnersleyNullVector];
Clear[KinnersleyNullTetrad]
KinnersleyNullTetrad[expr_,opts:OptionsPattern[]]:=KinnersleyNullVector[expr,#,opts]&/@{"l","n","m","mStar"}


Options[KinnersleyDerivative]={"Schwarzschild"->False};
Clear[KinnersleyDerivative]
KinnersleyDerivative[tt_Tensor?MetricQ,op_String,opts:OptionsPattern[]]:=
Module[{r,th,schw,t,phi,deriv},

	schw=OptionValue["Schwarzschild"];

	{t,r,th,phi}=Symbol/@{"t","r","\[Theta]","\[Phi]"};

	(Switch[op,
		"D",
		TensorValues@KinnersleyNullVector[tt,"l","Schwarzschild"->schw],

		"Delta",
		TensorValues@KinnersleyNullVector[tt,"n","Schwarzschild"->schw],

		"delta",
		TensorValues@KinnersleyNullVector[tt,"m","Schwarzschild"->schw],

		"deltaStar",
		TensorValues@KinnersleyNullVector[tt,"mStar","Schwarzschild"->schw],

		___,
		Print["No KinnersleyDerivative = "<>op];
		Print["Options are \"D\", \"Delta\", \"delta\", and \"deltaStar\"."];
		Abort[]

	].{D[#,t],D[#,r],D[#,th],D[#,phi]})&
]
KinnersleyDerivative["Schwarzschild",vec_String,opts:OptionsPattern[]]:=KinnersleyDerivative[ToMetric["Schwarzschild"],vec,"Schwarzschild"->True]
KinnersleyDerivative["Kerr",vec_String,opts:OptionsPattern[]]:=KinnersleyDerivative[ToMetric["Kerr"],vec,"Schwarzschild"->False]


Options[SpinCoefficient]={"Conjugate"->False,"Schwarzschild"->False};
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


Clear[ValidateIndices]
Tensor/:ValidateIndices[t_Tensor,{inds___}]:=
Module[{posInds,indsUp,repeatedInds},

	posInds=PossibleIndices[t];
	indsUp={inds}/.-sym_:>sym;
	repeatedInds=Cases[{inds},#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	If[Complement[indsUp,posInds]=!={},
		Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];
		Abort[]
	];
	If[Length[indsUp]=!=Total[Rank[t]],
		Print["The tensor ", t, " expects " ,Total[Rank[t]], " indices, but ", Length[indsUp], If[Length[indsUp]===1," index was ", " indices were "],"given."];
		Abort[]
	];
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		Abort[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],#[[1]]/.-sym_:>sym,##&[]]&/@repeatedInds];
		Abort[]
	];
]


Clear[ShiftIndices]
Options[ShiftIndices]={"SimplifyFunction"->Simplify};
Tensor/:ShiftIndices[t_Tensor,inds:{__},opts:OptionsPattern[]]:=
Module[{},
	ValidateIndices[t,inds];
	
	Fold[shiftIndex[#1,#2,OptionValue["SimplifyFunction"]]&,t,Thread[{Range@Length[inds],inds}]]
]


Clear[shiftIndex]
shiftIndex[t_Tensor,{pos_Integer,ind_},simpFn_]:=
Module[{gOrInvG,inds,indPos,indPosNew,tvs,indsBefore,indsAfter,n,itrBefore,itrAfter,vals,i,itrTot,itr,newPos,newMet,newInds},
	
	newPos=If[MatchQ[ind,_Symbol],"Up","Down"];
	indPos=IndexPositions[t];

	If[pos>Length@indPos,Print["Tensor ", t, " has only ",Length@indPos ," indices. Cannot raise at position ", pos,"."];Abort[]];
	indPosNew=ReplacePart[indPos,pos->newPos];
	inds=Indices[t];
	
	vals=
	If[indPos[[pos]]===newPos,
		TensorValues[t],
		
		If[TensorValues[TensorName[t],indPosNew]===Undefined,

			gOrInvG=TensorValues[If[newPos==="Up",InverseMetric[t],Metric[t]]];
			tvs=TensorValues[t];
			n=Dimensions[t];
			indsBefore=Table[itr[ii],{ii,1,pos-1}];
			indsAfter=Table[itr[ii],{ii,pos+1,Length@indPos}];
			itrBefore=({#,1,n}&/@indsBefore);
			itrAfter=({#,1,n}&/@indsAfter);
			itrTot=Join[itrBefore,{{i,1,n}},itrAfter];
			simpFn@Table[Sum[gOrInvG[[i,s]]tvs[[Sequence@@indsBefore,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]],
			
			TensorValues[TensorName[t],indPosNew]
		]
	];

	newInds=Flatten@{Take[inds,pos-1],ind,Drop[inds,pos]};
	newMet=If[MetricQ[t]&&(If[MatchQ[#,_Symbol],"Up","Down"]&/@newInds)==={"Down","Down"},"Self",Metric[t]];

	ToTensor[Join[KeyDrop[Association@@t,{"Indices","Metric"}],Association["Values"->vals,"Metric"->newMet,"Indices"->newInds]]]
]


Clear[ContractIndices]
ContractIndices[expr_]:=expr/.t_Tensor:>ContractIndices[t]
Tensor/:ContractIndices[t_Tensor]:=NestWhile[contractIndex,t,RepeatedIndexQ]
Tensor/:ContractIndices[t_Tensor,name_String]:=RenameTensor[ContractIndices[t],name]
Tensor/:ContractIndices[t_Tensor,{name_String,displayName_String}]:=RenameTensor[ContractIndices[t],{name,displayName}]


Clear[contractIndex]
contractIndex[t_Tensor]:=
Module[{indsUp,rptInd,rptIndsPos,indPos,indPosNew,inds,indsNew,tvsFull,n,vals,traceIndex,
	indsBefore,indsBetween,indsAfter,itrBefore,itrBetween,itrAfter,itrTot,tvs,itr},
	
	indPos=IndexPositions[t];
	inds=Indices[t];
	indsUp=inds/.-sym_:>sym;
	rptInd=First[If[Count[indsUp,#]===2,#,##&[]]&/@DeleteDuplicates@indsUp];
	rptIndsPos=Flatten@Position[indsUp,rptInd];

	indPosNew=Delete[indPos,{#}&/@Flatten@rptIndsPos];
	indsNew=Delete[inds,{#}&/@Flatten@rptIndsPos];

	vals=
		If[TensorValues[TensorName[t],indPosNew]===Undefined,

			tvs=TensorValues[t];
			n=Dimensions[t];
			indsBefore=Table[itr[ii],{ii,1,rptIndsPos[[1]]-1}];
			indsBetween=Table[itr[ii],{ii,rptIndsPos[[1]]+1,rptIndsPos[[2]]-1}];
			indsAfter=Table[itr[ii],{ii,rptIndsPos[[2]]+1,Length@indPos}];
			itrBefore=({#,1,n}&/@indsBefore);
			itrBetween=({#,1,n}&/@indsBetween);
			itrAfter=({#,1,n}&/@indsAfter);
			itrTot=Join[itrBefore,itrBetween,itrAfter];
			If[itrTot==={},
						Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],
						Table[Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]]
					],
						
			TensorValues[TensorName[t],indPosNew]
		];
	ToTensor[Join[KeyDrop[Association@@t,{"Indices","Name"}],Association["Name"->TensorName[t]<>"-Auto","Values"->vals,"Indices"->indsNew]]]
]


Tensor/:Component[t_Tensor,inds___List]:=
Module[{indsPos,indsAbstr,indsAbstrUp,coordsPos,indsUp},
	If[Length[inds]=!=Total@Rank[t],
		Print["Tensor ", t," expected ",Total@Rank[t]," indices to select a component, but ", Length[inds], If[Length[inds]===1," index was ", " indices were "],"given."];
		Abort[]
	];
	indsUp=inds/.-sym_:>sym;
	coordsPos=Flatten[Position[Coordinates[t],#]&/@indsUp];
	indsAbstrUp=Indices[t]/.-sym_:>sym;
	indsAbstr=MapThread[If[MatchQ[#1,_Symbol],#2,-#2]&,{inds,indsAbstrUp}];
	Part[TensorValues[t[Sequence@@indsAbstr]],Sequence@@coordsPos]
]


Clear[validateSumIndices]
validateSumIndices[inds1_List,inds2_List]:=
If[Sort[inds1]=!=Sort[inds2],
		Print["Cannot add Tensors with different indices, ",Sort[inds1]," and ",Sort[inds2]];
		Abort[]
	]



Clear[SumTensors]
Tensor/:SumTensors[t1_Tensor,t2_Tensor]:=
Module[{posInds,vals,inds,tvs,its,dims,itrs,local,indsLocal,indsFinal},

	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot sum Abstract Tensors."];Abort[]];
	If[TensorName@Metric[t1]=!=TensorName@Metric[t2],
		Print["Cannot sum Tensors with different metrics."];
		Print[TensorName@Metric[t1]];
		Print[TensorName@Metric[t2]];
		Abort[]
	];
	posInds=Union[PossibleIndices[t1],PossibleIndices[t2]];

	inds[1]=Indices[t1];
	inds[2]=Indices[t2];
	validateSumIndices[inds[1],inds[2]];

	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Sort@indsLocal[1];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@inds[1]);

	tvs[1]=TensorValues[t1];
	tvs[2]=TensorValues[t2];
	dims=Dimensions[t1];
	itrs={#,1,dims}&/@indsLocal["Tot"];
	
	vals=Table[tvs[1][[Sequence@@indsLocal[1]]]+tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]];

	ToTensor[{"("<>TensorName[t1]<>"+"<>TensorName[t2]<>")-Auto","("<>TensorDisplayName[t1]<>"+"<>TensorDisplayName[t2]<>")"},
			indsFinal,
			"Values"->vals,
			"Metric"->Metric[t1],
			"IsMetric"->False,
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->posInds,
			"Dimensions"->Dimensions[t1]]
]
Tensor/:SumTensors[t1_Tensor]:=t1;
Tensor/:SumTensors[t1_Tensor,t2__Tensor]:=Fold[SumTensors,t1,{t2}]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,name_String]:=RenameTensor[SumTensors[t1,t2],name]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=RenameTensor[SumTensors[t1,t2],{name,displayName}]


Clear[validateProductIndices]
validateProductIndices[inds1_List,inds2_List]:=
Module[{indsUp,repeatedInds,inds},

	inds=Join[inds1,inds2];
	indsUp=ToCovariant[inds];
	repeatedInds=Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		Abort[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],ToCovariant[#[[1]]],##&[]]&/@repeatedInds];
		Abort[]
	];
]


Clear[MultiplyTensors]
Tensor/:MultiplyTensors[t1_Tensor,t2_Tensor]:=
Module[{posInds,vals,inds,repeatedInds,tvs,dims,itrs,indsLocal,local,indsFinal},

	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[TensorName@Metric[t1]=!=TensorName@Metric[t2],Print["Cannot multiply Tensors with different metrics."];Abort[]];
	posInds=Union[PossibleIndices[t1],PossibleIndices[t2]];
	
	inds[1]=Indices[t1];
	inds[2]=Indices[t2];
	validateProductIndices[inds[1],inds[2]];
	
	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Sort@Join[indsLocal[1],indsLocal[2]];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@Join[inds[1],inds[2]]);

	tvs[1]=TensorValues[t1];
	tvs[2]=TensorValues[t2];
	dims=Dimensions[t1];
	itrs={#,1,dims}&/@indsLocal["Tot"];
	vals=Table[tvs[1][[Sequence@@indsLocal[1]]]tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]];

	ToTensor[{"("<>TensorName[t1]<>"\[CenterDot]"<>TensorName[t2]<>")-Auto","("<>TensorDisplayName[t1]<>"\[CenterDot]"<>TensorDisplayName[t2]<>")"},
			indsFinal,
			"Values"->vals,
			"Metric"->Metric[t1],
			"IsMetric"->False,
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->posInds,
			"Dimensions"->dims]
]

Tensor/:MultiplyTensors[t1_Tensor]:=t1;
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor]:=Fold[MultiplyTensors,t1,{t2}]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,name_String]:=RenameTensor[MultiplyTensors[t1,t2],name]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=RenameTensor[MultiplyTensors[t1,t2],{name,displayName}]


Clear[MultiplyTensorScalar]
Tensor/:MultiplyTensorScalar[t_Tensor,n_]:=MultiplyTensorScalar[n,t];
Tensor/:MultiplyTensorScalar[n_,t_Tensor]:=
Module[{vals},
	If[AbstractQ[t],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[Not[MatchQ[n,(_Symbol|_Real|_Complex|_Integer|_Rational|_Times|_Plus)]],Print["Cannot multiply a Tensor by a ", Head[n]];Abort[]];
	vals=n TensorValues[t];

	ToTensor[{"("<>ToString[n]<>TensorName[t]<>")-Auto","("<>ToString[n]<>"\[CenterDot]"<>TensorDisplayName[t]<>")"},
			Indices[t],
			"Values"->vals,
			"Metric"->Metric[t],
			"IsMetric"->False,
			"Coordinates"->Coordinates[t],
			"Abstract"->False,
			"PossibleIndices"->PossibleIndices[t],
			"Dimensions"->Dimensions[t]]

]
Tensor/:MultiplyTensorScalar[t1_Tensor]:=t1;
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,name_String]:=RenameTensor[MultiplyTensorScalar[n,t1],name]
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,{name_String,displayName_String}]:=RenameTensor[MultiplyTensorScalar[n,t1],{name,displayName}]


Tensor/:D[t1_Tensor,a_]:=
Module[{vals,inds,repeatedInds,tvs,dims,itrs,indsLocal,local,indsFinal,coords},

	inds[1]={a};
	inds[2]=Indices[t1];
	validateProductIndices[inds[1],inds[2]];
	
	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Join[indsLocal[1],indsLocal[2]];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@Join[inds[1],inds[2]]);

	tvs=TensorValues[t1];
	dims=Dimensions[t1];
	coords=Coordinates[t1];
	itrs={#,1,dims}&/@indsLocal["Tot"];
	vals=Table[D[tvs[[Sequence@@indsLocal[2]]],coords[[Sequence@@indsLocal[1]]]],Evaluate[Sequence@@itrs]];

	ToTensor[{"(PartialD"<>TensorName[t1]<>")-Auto","(\[PartialD]"<>TensorDisplayName[t1]<>")"},
			indsFinal,
			"Values"->vals,
			"Metric"->Metric[t1],
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->PossibleIndices[t1],
			"Dimensions"->dims]
]

Tensor/:MultiplyTensors[t1_Tensor]:=t1;
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor]:=Fold[MultiplyTensors,t1,{t2}]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,name_String]:=RenameTensor[MultiplyTensors[t1,t2],name]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=RenameTensor[MultiplyTensors[t1,t2],{name,displayName}]


Tensor/:ActOnTensorValues[t_Tensor,fn_]:=SetTensorValues[t,fn@TensorValues[t]]


Options[MergeTensors]={"SimplifyFunction"->Identity};
Clear[MergeTensors]
MergeTensors[expr_,opts:OptionsPattern[]]:=
Module[{expr1,expr2,simpFn},
	simpFn=OptionValue["SimplifyFunction"];
	expr1=Expand[expr]/.t1_Tensor t2__Tensor:>MultiplyTensors[t1,t2];
	expr2=expr1//.n_ t_Tensor/;Not[MatchQ[n,_Tensor]]:>MultiplyTensorScalar[n,t];
	ActOnTensorValues[ContractIndices[expr2]/.Plus[t1_Tensor,t2__Tensor]:>SumTensors[t1,t2],simpFn]
]
MergeTensors[expr_,name_String,opts:OptionsPattern[]]:=RenameTensor[MergeTensors[expr,opts],name]
MergeTensors[expr_,{name_String,dispName_String},opts:OptionsPattern[]]:=RenameTensor[MergeTensors[expr,opts],{name,dispName}]


Clear[ClearCachedTensorValues]
ClearCachedTensorValues[s_String,inds_]:=If[TensorValues[s,inds]=!=Undefined,Unset[TensorValues[s,inds]]]
ClearCachedTensorValues[t_Tensor]:=Scan[ClearCachedTensorValues[TensorName[t],#]&,Tuples[{"Up","Down"},Total[Rank[t]]]]
ClearCachedTensorValues[All]:=Scan[ClearCachedTensorValues[Sequence@@#]&,DeleteDuplicates@Cases[DownValues[TensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[TensorValues][x__]]:>{x},{_String,{___String}}]]


Clear[CachedTensorValues]
CachedTensorValues[s_String]:=#->TensorValues@@#&/@(Cases[DownValues[TensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[TensorValues][x__]]:>{x},{s,{___String}}])
CachedTensorValues[t_Tensor]:=CachedTensorValues[TensorName[t]]
CachedTensorValues[All]:=CachedTensorValues/@DeleteDuplicates@Cases[DownValues[TensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[TensorValues][x__]]:>{x},{n_String,{___String}}:>n]


AutoNameQ[t_Tensor]:=AutoNameQ[TensorName[t]]
AutoNameQ[s_String]:=StringMatchQ[s,__~~"-Auto"]


Clear[SetTensorKeyValue]
Tensor/:SetTensorKeyValue[t_Tensor,key_String,value_]:=ToTensor[Join[KeyDrop[(Association@@t),{key}],Association[key->value]]]


Clear[SetMetric]
Tensor/:SetMetric[t_Tensor,m_Tensor]:=If[t=!=m,SetTensorKeyValue[t,"Metric",m],t]


Clear[SetAsMetric]
Tensor/:SetAsMetric[t_Tensor,tf_?BooleanQ]:=SetTensorKeyValue[t,"Metric",tf]


Clear[SetIndices]
Tensor/:SetIndices[t_Tensor,inds___List]:=SetTensorKeyValue[t,"Indices",inds]


Clear[SetPossibleIndices]
Tensor/:SetPossibleIndices[t_Tensor,inds_List]:=SetTensorKeyValue[t,"PossibleIndices",inds]


Clear[SetCoordinates]
Tensor/:SetCoordinates[t_Tensor,coords_List]:=SetTensorKeyValue[t,"Coordinates",coords]


Clear[SetTensorName]
Tensor/:SetTensorName[t_Tensor,{name_String,dispName_String}]:=SetTensorDisplayName[SetTensorKeyValue[t,"Name",name],dispName]
Tensor/:SetTensorName[t_Tensor,name_String]:=SetTensorName[t,{name,name}]


Clear[SetTensorDisplayName]
Tensor/:SetTensorDisplayName[t_Tensor,name_String]:=SetTensorKeyValue[t,"DisplayName",name]


Clear[RenameTensor]
RenameTensor=SetTensorName;


Clear[SetAsAbstract]
Tensor/:SetAsAbstract[t_Tensor,tf_?BooleanQ]:=SetTensorKeyValue[t,"Abstract",tf]


Clear[SetTensorValues]
Tensor/:SetTensorValues[t_Tensor,values_List]:=SetTensorKeyValue[t,"Values",values]
Tensor/:SetTensorValues[t_Tensor,values_]/;Rank[t]==={0,0}:=SetTensorKeyValue[t,"Values",values]


Clear[ToCovariant]
ToCovariant[inds_]:=inds/.-sym_:>sym


CovariantD[t_Tensor,a_]:=
Module[{chr,inds,dummies,tvs,coords,dims},
 
	chr=ChristoffelSymbol[Metric[t]];
	inds=Indices[t];
	dummies=Complement[PossibleIndices[t],inds/.{-sym_:>sym}];
	tvs=TensorValues[t];
	coords=Coordinates[t];
	dims=Dimensions[t];

	D[t,a]+Sum[chrTerm[t,i,a],{i,Indices[t]}]
]


chrTerm[t_Tensor,tensorInd_,derivInd_]:=
Module[{inds,dummy,chr,chrDummy,newInds,tNew,tensorIndUp},
	inds=Indices[t];
	tensorIndUp=ToCovariant[tensorInd];
	dummy=First[Complement[PossibleIndices[t],ToCovariant@Join[{tensorInd,derivInd},inds]]];
	chr=ChristoffelSymbol[Metric[t]];
	chrDummy=If[MatchQ[tensorInd,-_Symbol],chr[dummy,tensorInd,derivInd],chr[tensorInd,-dummy,derivInd]];

	newInds=inds/.tensorIndUp->dummy;
	tNew=ToTensor[Join[KeyDrop[(Association@@t),{"Indices","Metric","IsMetric"}],Association["Indices"->newInds,"Metric"->Metric[t],"IsMetric"->False]]];
	If[tensorIndUp===tensorInd,1,-1]tNew chrDummy
]


Clear[FourVelocity]
FourVelocity[tens_Tensor?MetricQ]:=
Module[{t,r,th,ph,tau},

	{t,r,th,ph,tau}=Symbol/@{"t","r","\[Theta]","\[Phi]","\[Tau]"};
	ToTensor[{"FourVelocity"<>TensorName[tens],"u"},tens,{t[tau],r[tau],th[tau],ph[tau]}]
]

FourVelocity["Schwarzschild"]:=
Module[{t,rp,EE,JJ,M},
	{t,rp,EE,JJ,M}=Symbol/@{"t","rp","\[ScriptCapitalE]","\[ScriptCapitalJ]","M"};
	SetTensorValues[FourVelocity[ToMetric["Schwarzschild"]],{EE/(1-(2 M)/rp[t]),(EE rp'[t])/(1-(2 M)/rp[t]),0,JJ/rp[t]^2}]
]



Clear[TensorHarmonic]
TensorHarmonic[label_]:=
Module[{Ylm,YAVal,lTemp,mTemp,thTemp,phTemp,l,m,th,ph,A,B,F,G,eps},

{Ylm,l,m,th,ph,A,B,F,G}=Symbol/@{"Ylm","l","m","\[Theta]","\[Phi]","A","B","F","G"};

YAVal=Simplify[{D[Ylm[lTemp,mTemp,thTemp,phTemp],thTemp],D[Ylm[l,m,thTemp,phTemp],phTemp]},{\[Pi]>=thTemp>=0,2\[Pi]>=phTemp>=0}]/.{lTemp->l,mTemp->m,thTemp->th,phTemp->ph};
eps=LeviCivitaSymbol["TwoSphere"];

Switch[label,
	"YA",
	ToTensor[{"HarmonicYA","Y"},ToMetric["TwoSphere"],YAVal,{-A}],
	"XA",
	ContractIndices[MergeTensors[-eps[-A,F]TensorHarmonic["YA"][-F]],{"HarmonicXA","X"}],
	"YAB",
	MergeTensors[CovariantD[TensorHarmonic["YA"][-B],-A]
				+1/2 l(l+1)Ylm[l,m,th,ph]ToMetric["TwoSphere"][-A,-B],{"HarmonicYAB","Y"}],
	"XAB",
	MergeTensors[-(1/2)(eps[-G,F]CovariantD[TensorHarmonic["YA"][-F],-B]
			+eps[-B,F]CovariantD[TensorHarmonic["YA"][-F],-G]),{"HarmonicXAB","X"}][-A,-B],
	___,
	Print["No TensorHarmonic associated with label ", label];
	Print["Options are: ",{"YA","XA","YAB","XAB"}];
]
]


End[];

EndPackage[];
