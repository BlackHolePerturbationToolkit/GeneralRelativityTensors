(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`BasicTensors`",{"GeneralRelativityTensors`Utils`"}];


Tensor::usage="Tensor is a Head created with the command ToTensor.";


ToTensor::usage="ToTensor[n,m,vals,inds] returns a Tensor with indices \
inds and TensorName n. The metric m and values vals \
(given as a consistently sized List) are assigned.
ToTensor[n,m,vals] is equivalent, but assumes all contravariant indices.";

ToMetric::usage="ToMetric[n,coords,vals,posInds] returns a metric Tensor with TensorName \
n, Coordinates coords, TensorValues vals, and PossibleIndices posInds.
ToMetric[builtIn] returns a built-in metric Tensor, where builtIn is a String such as \"Schwarzschild\".";
InverseMetric::usage="InverseMetric[t] returns the inverse metric Tensor associated with the Tensor \
t, or Undefined if no metric was set. If t is on a curve, InverseMetric[t] returns \
the inverse metric Tensor on the same curve.";
Metric::usage="Metric[expr] returns the metric Tensor associated with the Tensor expression expr.";
MetricQ::usage="MetricQ[t] returns True if the Tensor t is a metric.";
MultipleMetricsQ;
SingleMetricQ;

ManifoldDimensions::usage="ManifoldDimensions[t] returns the number of manifold dimensions in the \
manifold of Tensor t."
Coordinates::usage="Coordinates[expr] returns a List of symbols used for the coordinates of the Tensor \
expression expr.";
Rank::usage="Rank[t] returns the Tensor rank of the Tensor t as a List {p,q}, \
where p is the number of contravariant indices and q the number of covariant indices.";
Indices::usage="Indices[t] returns a List of Symbols representing the indices of the Tensor t. \
Positive Symbols are contravariant and negative Symbols are covariant.
Indices[te] returns the list of indices associated with the TensorExpression te. \
Indices[expr] will return a uniqe list of indices if each term in the Tensor expression expr \
has the same indices.";
IndicesTraced::usage="IndicesTraced[expr] returns a unique list of indices that each term \
in the Tensor expression expr would have if all dummy indices were traced out.";
PossibleIndices::usage="PossibleIndices[expr] returns a List of all possible Symbols that can \
represent the indices of Tensors in the Tensor expression expr.";
IndexPositions::usage="IndexPositions[t] returns a List of elements \
\"Up\" and \"Down\" which represent (respectively) the contravariant and covariant positions of the \
indices of Tensor t.";

TensorName::usage="TensorName[t] returns the name of Tensor \
t which is used for storing cached values in the Symbol RawTensorValues.";
TensorDisplayName::usage="TensorDisplayName[t] returns the name of \
Tensor t that is used for formatted output.";


SetTensorKeyValue::usage="SetTensorKeyValue[t,key,value] returns the Tensor t with the appropriate Rule changed to key->value.";
SetTensorName::usage="SetTensorName[t,n] returns the Tensor t with its TensorName changed to n.";
SetTensorDisplayName::usage="SetTensorDisplayName[t,n] returns the Tensor t with its TensorDisplayName changed to n.";
SetTensorValues::usage="SetTensorValues[t,vals] returns the Tensor t with its RawTensorValues set to vals.";
SetCoordinates::usage="SetCoordinates[t,coords] returns the Tensor t with its Coordinates set to coords.";
SetIndices::usage="SetIndices[t,inds] returns the Tensor t with its Indices set to inds.";
SetPossibleIndices::usage="SetPossibleIndices[t,posInds] returns the Tensor t with its PossibleIndices set to posInds.";
SetMetric::usage="SetMetric[t,m] returns the Tensor t with its Metric set to m.";
SetMetricQ::usage="SetMetricQ[t,bool] returns the Tensor t with its MetricQ flag set to bool (True or False).";


RawTensorValues::usage="RawTensorValues[t] returns the values actually stored inside the Tensor t.
RawTensorValues[n,inds] returns the cached values of a Tensor \
with TensorName n and indices in positions inds or \
Undefined if none have been computed. The List inds should contain elements \"Up\" and/or \"Down\".";
TensorValues::usage="TensorValues[t] returns the RawTensorValues of t. \
If t has an associated curve, the values are evaluated along the curve.";


ClearCachedTensorValues::usage="ClearCachedTensorValues[n,inds] removes cached expressions stored with \
the Symbol RawTensorValues using the TensorName n and IndexPositions inds. \
Here inds is a List of \"Up\" and \"Down\".
ClearCachedTensorValues[n] removes all cached expressions stored with the Symbol \
RawTensorValues for any Tensor with name n.
ClearCachedTensorValues[t] removes all cached expressions stored with the Symbol \
RawTensorValues for the Tensor t.
ClearCachedTensorValues[All] removes all cached expressions associated with the Symbol RawTensorValues.";
CachedTensorValues::usage="CachedTensorValues[n] returns a List of Rules showing all cached expressions \
for the TensorName n (stored in the Symbol RawTensorValues).
CachedTensorValues[t] returns a List of Rules showing all cached expressions \
for the Tensor t (stored in the Symbol RawTensorValues).
CachedTensorValues[All] returns a List of Rules showing all cached expressions (stored in the Symbol RawTensorValues)."
$CacheTensorValues::usage="$CacheTensorValues is a global boolean (with default value False) specifying whether to cache Tensor values in the symbol RawTensorValues."
ActOnTensorValues::usage="ActOnTensorValues[f,t] acts with the functions f on the values of Tensor t and returns the resulting tensor.";


BuiltInIndices;


Begin["`Private`"];


Options[SetTensorKeyValue]={"IgnoreWarnings"->False};
Options[SetMetricQ]=Options[SetTensorKeyValue];
Options[SetMetric]=Options[SetTensorKeyValue];
Options[SetIndices]=Options[SetTensorKeyValue];
Options[SetPossibleIndices]=Options[SetTensorKeyValue];
Options[SetCoordinates]=Options[SetTensorKeyValue];
Options[SetTensorName]=Options[SetTensorKeyValue];
Options[SetTensorDisplayName]=Options[SetTensorKeyValue];
Options[SetTensorValues]=Options[SetTensorKeyValue];

DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"] = {"IgnoreWarnings"->"If True, the Tensor's key value will be set regardless of whether it violates built-in warnings."};
DocumentationBuilder`OptionDescriptions["SetMetricQ"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetMetric"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetIndices"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetPossibleIndices"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetSubmanifoldIndices"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCoordinates"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTensorName"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTensorDisplayName"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTensorValues"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];


$CacheTensorValues=False;


Tensor/:Format[t_Tensor]:=formatTensor[TensorDisplayName@t,Indices@t]


Clear[indicesStrings]
indicesStrings[inds_]:=
Module[{dnStr,upStr},
	dnStr=StringJoin[If[MatchQ[#,-_Symbol],ToString[#/.-x_:>x],StringJoin@Table["  ",StringLength[ToString[#]]]]&/@inds];
	upStr=StringJoin[If[Not@MatchQ[#,-_Symbol],ToString[#],StringJoin@Table["  ",StringLength[ToString[#/.-x_:>x]]]]&/@inds];

	{upStr,dnStr}
]


Clear[formatTensorBoxes]
formatTensorBoxes[name_,inds_]:=
Module[{upStr,dnStr,out1,nameStr},
	nameStr = If[MatchQ[name,_String],name,ToString[name]];
	If[inds==={},
		nameStr,
		{upStr,dnStr}=indicesStrings[inds];
		SubsuperscriptBox[nameStr,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
	]
]


Clear[formatTensor]
formatTensor[name_,inds_]:=DisplayForm@formatTensorBoxes[name,inds]


Clear[multiMetricFunction]
multiMetricFunction[t_]:=If[MultipleMetricsQ[t],#,First[#]]&;


Options[ManifoldDimensions]={"PerIndex"->False};
reDef@ManifoldDimensions[t_Tensor,opts:OptionsPattern[]]:=
Module[{tests,perI,allCoords},
	tests = {"PerIndex" ->{BooleanQ,"PerIndex must be True or False."}};
	TestOptions[tests,{opts}];
	perI=OptionValue["PerIndex"];
	allCoords=(Association@@t)["Coordinates"];

	If[MultipleMetricsQ[t]||perI,
		Length/@allCoords,
		Length[allCoords[[1]]]
	]
]


Options[Coordinates]=Options[ManifoldDimensions];
reDef@Coordinates[t_Tensor,opts:OptionsPattern[]]:=
Module[{tests,perI,allCoords},
	tests = {"PerIndex" ->{BooleanQ,"PerIndex must be True or False."}};
	TestOptions[tests,{opts}];
	perI=OptionValue["PerIndex"];
	allCoords=(Association@@t)["Coordinates"];

	If[MultipleMetricsQ[t]||perI,
		allCoords,
		allCoords[[1]]
	]
]


Options[PossibleIndices]=Options[ManifoldDimensions];
reDef@PossibleIndices[t_Tensor,opts:OptionsPattern[]]:=
Module[{tests,perI,allPosInds},
	tests = {"PerIndex" ->{BooleanQ,"PerIndex must be True or False."}};
	TestOptions[tests,{opts}];
	perI=OptionValue["PerIndex"];
	allPosInds=(Association@@t)["PossibleIndices"];

	If[MultipleMetricsQ[t]||perI,
		allPosInds,
		allPosInds[[1]]
	]
]


Options[Metric]=Options[ManifoldDimensions];
reDef@Metric[t_Tensor,opts:OptionsPattern[]]:=
Module[{tests,perI,allMetrics},
	tests = {"PerIndex" ->{BooleanQ,"PerIndex must be True or False."}};
	TestOptions[tests,{opts}];
	perI=OptionValue["PerIndex"];
	allMetrics=(Association@@t)["Metric"];
	
	If[allMetrics==="Self",
		If[perI,{t,t},t],
		If[MultipleMetricsQ[t]||perI,
			allMetrics,
			allMetrics[[1]]
		]
	]
]


reDef@Rank[t_Tensor]:=Module[{inds,co},inds=Indices[t];co=Count[inds,-_Symbol];{Length[inds]-co,co}];


reDef@Indices[t_Tensor]:=(Association@@t)["Indices"];
def@TensorName[t_Tensor]:=(Association@@t)["Name"];
reDef@TensorName[Undefined]:=Undefined;
def@TensorDisplayName[t_Tensor]:=(Association@@t)["DisplayName"];
reDef@IndexPositions[expr_]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[expr];


reDef@MetricQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["MetricQ"];
def@MultipleMetricsQ[t_]:=If[(MetricQ[t]||(Length[TensorName/@(Association@@t)["Metric"]]===1)),False,MatchQ[t,_Tensor]&&(Not[SameQ@@(TensorName/@(Association@@t)["Metric"])])]
def@SingleMetricQ[t_]:=Not@MultipleMetricsQ[t]


basicTensorKeys={"Coordinates","DisplayName","Indices","Metric",
				"MetricQ","Name","PossibleIndices","Values"};


testDef@
toTensorTests[assoc_Association]:=
Module[{stringKeys,booleanKeys,dims,lens,tfList},
	stringKeys={"Name","DisplayName"};
	booleanKeys={"MetricQ"};
	
	If[Complement[basicTensorKeys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensor formation: "<>ToString[Complement[basicTensorKeys,Keys[assoc]]]];
		AbortVerbose[]
	];
	
	ValidateIndices[assoc["Indices"]];
	
	lens = Length/@{assoc["Coordinates"],assoc["Indices"],assoc["PossibleIndices"]};
	If[Not[SameQ@@lens],
		Print["Inconsistent data. When forming a Multiple-Metric Tensor, the number of lists of Coordinates, \
Indices, and PossibleIndices must be the same."];
		Print["Number of lists of ", #, " = ", Length[assoc[#]]]&/@{"Coordinates","Dimensions","Indices","PossibleIndices"};
		AbortVerbose[];
	];
	
	If[assoc["Metric"]=!="Self"&&Length[assoc["Metric"]]=!=First@lens,
		Print["List of given metrics is ", Length[assoc["Metric"]] ," long but other values given as lists that are ", First@lens ," long."];
		AbortVerbose[];
	];

	If[Not[MatchQ[assoc/@stringKeys,{_String..}]],
		Print["The following values were not given as Strings: "<>ToString[If[Head[assoc[#]]=!=String,#,Nothing]&/@stringKeys]];
		AbortVerbose[]
	];
		
	If[Not@MatchQ[assoc/@booleanKeys,{_?BooleanQ..}],
		Print["The following values were not given as Booleans: "<>ToString[If[Not[BooleanQ[assoc@#]],#,Nothing]&/@booleanKeys]];
		AbortVerbose[]
	];
	
	If[Not@MatchQ[assoc["Coordinates"],{{__Symbol|__Integer}..}],
		Print["Coordinates must be a List of Lists of Symbols or Integers."];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["Indices"]/.-sym_Symbol:>sym,{___Symbol}],
		Print["Indices must be a list of Symbols (and negative Symbols)."];
		AbortVerbose[]
	];
	
	If[Not@MatchQ[assoc["PossibleIndices"],{{__Symbol}..}]||Min[Length/@assoc["PossibleIndices"]]<8,
		Print["PossibleIndices must be a list of at least 8 Symbols. Given here as: ",assoc["PossibleIndices"]];
		AbortVerbose[]
	];
	
	(*tfList=MapThread[MemberQ[#2,#1/.(-a_Symbol:>a)]&,{assoc["Indices"],assoc["PossibleIndices"]}];*)
	tfList=MapThread[memberUniqueQ[#2,#1/.(-a_Symbol:>a)]&,{assoc["Indices"],assoc["PossibleIndices"]}];
	If[assoc["Indices"]=!={}&&DeleteDuplicates[tfList]=!={True},
		Print["Not all Indices are given in their lists of PossibleIndices."];
		MapThread[If[Not@#3,Print["Index: ", #1, ", PossibleIndices: ",#2],Nothing]&,{assoc["Indices"],assoc["PossibleIndices"],tfList}];
		AbortVerbose[]
	];
	
	tfList=MapThread[Intersection[#2,#1]==={}&,{assoc["Coordinates"],assoc["PossibleIndices"]}];
	If[assoc["Indices"]=!={}&&DeleteDuplicates[tfList]=!={True},
		Print["Coordinate symbols also found in PossibleIndices lists."];
		MapThread[If[Not@#3,Print["Coordinates: ", #1, ", PossibleIndices: ",#2],Nothing]&,{assoc["Coordinates"],assoc["PossibleIndices"],tfList}];
		AbortVerbose[]
	];
	
	dims[expr_]:=If[MatchQ[expr,_List],Dimensions[expr],{}];
	If[dims[assoc["Values"]]=!=Length/@assoc["Coordinates"],
		Print["Provided values are inconsistent with given Tensor Rank and/or Dimensions."];
		AbortVerbose[]
	];
	
	(* We could use some tests for the metric here *)
]


def@
ToTensor[assoc_Association]:=
Module[{basicTestKeys},

	toTensorTests[assoc];
	
	If[#=!=Undefined&&Not[AutoNameQ[assoc["Name"]]]&&$CacheTensorValues,RawTensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];
	Tensor@@(Normal@assoc)
]


reDef@
ToTensor[{name_String,dispName_String},{mets__Tensor?MetricQ},vals_,indsGiven_:Undefined]:=
Module[{coords,posInds,dims,inds,assoc},

	coords=Coordinates/@{mets};	
	posInds=PossibleIndices/@{mets};
	dims=ManifoldDimensions/@{mets};
	inds=If[indsGiven===Undefined,multiMetricTensorIndices[{mets}],indsGiven];
	
	assoc = Association["Coordinates"->coords,
						"DisplayName"->dispName,
						"Indices"->inds,
						"MetricQ"->False,
						"Metric"->{mets},
						"Name"->name,
						"PossibleIndices"->posInds,
						"Values"->vals];
	
	ToTensor[KeySort@assoc]
];
reDef@ToTensor[name_String,{mets__Tensor?MetricQ},vals_,indsGiven_:Undefined]:=ToTensor[{name,name},{mets},vals,indsGiven];


reDef@
ToTensor[{name_String,dispName_String},metric_Tensor?MetricQ,vals_,indsGiven_:Undefined]:=
Module[{nInds},
	nInds=If[MatchQ[vals,_List],Length@Dimensions[vals],0];
	ToTensor[{name,dispName},Table[metric,nInds],vals,indsGiven]
];
reDef@ToTensor[name_String,metric_Tensor?MetricQ,vals_,indsGiven_:Undefined]:=ToTensor[{name,name},metric,vals,indsGiven];


testDef@
BuiltInIndices[label_String]:=
Switch[label,
		"Latin",
		Symbol/@Complement[CharacterRange["a","z"],{"r","t"}],
		"CapitalLatin",
		Symbol/@Complement[CharacterRange["A","Z"],{"D","C","E","I","K","N","O"}],
		"Greek",
		Symbol/@Complement[CharacterRange["\[Alpha]","\[Omega]"],{"\[Pi]","\[Theta]","\[Phi]","\[Tau]","\[Chi]"}],
		___,
		Print["No built-in indices ", label, ". Options are \"Latin\", \"CapitalLatin\", and \"Greek\""]; AbortVerbose[]
]


testDef@
multiMetricTensorIndices[{mets__Tensor?MetricQ}]:=
Module[{nextIndex,metNames,namesInds,indNamesInds},

	nextIndex[indsNamesInds_,metName_]:=
	Module[{newInd,oldInds,namesIndsList},
		{oldInds,namesIndsList}=indsNamesInds;
		newInd=Select[namesIndsList,#[[1]]===metName&][[1,2,1]];
		{Join[{newInd},oldInds],namesIndsList/.{metName,posInds_}:>{metName,Rest@posInds}}
	];

	metNames=TensorName/@{mets};
	namesInds={TensorName[#],PossibleIndices[#]}&/@{mets};
	indNamesInds={{},DeleteDuplicates@namesInds};
	Reverse@First@Fold[nextIndex,indNamesInds,metNames]
]


reDef@
ToMetric[{name_String,dispName_String},coords_List,vals_List,posIndsParam_]:=
Module[{inds,posInds,posIndsFull,dims},

	posIndsFull = 
		If[MemberQ[{"Greek","Latin","CapitalLatin"},posIndsParam],
			BuiltInIndices[posIndsParam],
			If[Not[MatchQ[posIndsParam,{__Symbol}]]||Length[posIndsParam]<8,
				Print["At least 8 PossibleIndices needed when defining a Metric"];
				AbortVerbose[],
			
				posIndsParam
			]
		];

	posInds=Complement[posIndsFull,coords];
	inds=-Take[posInds,2];
	
	If[Not@MatchQ[coords,{__Symbol|__Integer}],
		Print["Metric Coordinates not given as a List of Symbols or Integers"];
		AbortVerbose[]
	];

	dims=Length@coords;
	
	If[Dimensions[vals]=!={dims,dims},
			Print["To be consistent with the number of given Coordinates, Metric values must be a ", 
				dims, " \[Times] ", dims, " matrix given as a nested List."];
			AbortVerbose[]
	];
					
	ToTensor[KeySort@Association[
					"Coordinates"->{coords,coords},
					"DisplayName"->dispName,
					"Indices"->inds,
					"Metric"->"Self",
					"MetricQ"->True,
					"Name"->name,
					"PossibleIndices"->{posInds,posInds},
					"Values"->vals]]
];
reDef@ToMetric[{name_String,dispName_String},coords_,vals_]:=ToMetric[{name,dispName},coords,vals,"Greek"]
reDef@ToMetric[name_String,coords_,vals_,posIndsParam_]:=ToMetric[{name,name},coords,vals,posIndsParam]
reDef@ToMetric[name_String,coords_,vals_]:=ToMetric[{name,name},coords,vals,"Greek"]


Options[InverseMetric]=Options[ManifoldDimensions];
reDef@
InverseMetric[t_Tensor?MetricQ,opts:OptionsPattern[]]:=InverseMetric[t,opts]=
Module[{assoc,tv,tests,perI,ig},
	tests = {"PerIndex" ->{BooleanQ,"PerIndex must be True or False."}};
	TestOptions[tests,{opts}];
	perI=OptionValue["PerIndex"];

	tv=Simplify[Inverse@RawTensorValues[Metric[t]]];
	
	assoc=Association@@t;
	ig=ToTensor[KeySort@Join[assoc,
					Association["Indices"->Indices[t]/.-sym_Symbol:>sym,
								"Values"->tv,
								"Metric"->{Metric[t],Metric[t]}]]];
								
	If[perI,{ig,ig},ig]
];

reDef@
InverseMetric[t_Tensor,opts:OptionsPattern[]]:=
Module[{tests,perI},
	tests = {"PerIndex" ->{BooleanQ,"PerIndex must be True or False."}};
	TestOptions[tests,{opts}];
	perI=OptionValue["PerIndex"];

	If[MultipleMetricsQ[t]||perI,
		InverseMetric[#,"PerIndex"->False]&/@Metric[t,"PerIndex"->True],
		InverseMetric[Metric[t,"PerIndex"->False],"PerIndex"->False]
	]
]


def@
ActOnTensorValues[fn_,t_Tensor]:=SetTensorValues[t,Map[fn,RawTensorValues[t],{Total@Rank[t]}]]


def@
ClearCachedTensorValues[s_String,inds_]:=If[RawTensorValues[s,inds]=!=Undefined,Unset[RawTensorValues[s,inds]]]
reDef@
ClearCachedTensorValues[str_String]:=Scan[ClearCachedTensorValues@@#&,Cases[CachedTensorValues[All],HoldPattern[{str,a___}->_]:>{str,a},Infinity]]
reDef@
ClearCachedTensorValues[t_Tensor]:=Scan[ClearCachedTensorValues[TensorName[t],#]&,Tuples[{"Up","Down"},Total[Rank[t]]]]
reDef@
ClearCachedTensorValues[All]:=Scan[ClearCachedTensorValues[Sequence@@#]&,
									DeleteDuplicates@Cases[DownValues[RawTensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[RawTensorValues][x__]]:>{x},{_String,{___String}}]]


def@
CachedTensorValues[s_String]:=#->RawTensorValues@@#&/@(Cases[DownValues[RawTensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[RawTensorValues][x__]]:>{x},{s,{___String}}])
reDef@
CachedTensorValues[t_Tensor]:=CachedTensorValues[TensorName[t]]
reDef@
CachedTensorValues[All]:=CachedTensorValues/@DeleteDuplicates@Cases[DownValues[RawTensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[RawTensorValues][x__]]:>{x},{n_String,{___String}}:>n]


def@AutoNameQ[t_Tensor]:=AutoNameQ[TensorName[t]]
reDef@AutoNameQ[s_String]:=StringMatchQ[s,__~~"-Auto"]


def@
SetTensorKeyValue[t_Tensor,key_String,value_,opts:OptionsPattern[]]:=
Module[{tests},
	tests = {"IgnoreWarnings" ->{BooleanQ,"IgnoreWarnings of MergeNested must be True or False."}};
	TestOptions[tests,{opts}];

	If[OptionValue["IgnoreWarnings"],
		Tensor[Normal[KeySort@Join[KeyDrop[(Association@@t),{key}],Association[key->value]]]],
		ToTensor[KeySort@Join[KeyDrop[(Association@@t),{key}],Association[key->value]]]
	]
]


def@
SetMetric[t_Tensor,m_Tensor,opts:OptionsPattern[]]:=If[t=!=m,SetTensorKeyValue[t,"Metric",m,opts],t]


def@
SetMetricQ[t_Tensor,bool_?BooleanQ,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"MetricQ",bool,opts]


def@
SetIndices[t_Tensor,inds___List,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Indices",inds,opts]


def@
SetPossibleIndices[t_Tensor,inds_List,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"PossibleIndices",inds,opts]


def@
SetCoordinates[t_Tensor,coords_List,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Coordinates",coords,opts]


def@
SetTensorName[t_Tensor,{name_String,dispName_String},opts:OptionsPattern[]]:=SetTensorDisplayName[SetTensorKeyValue[t,"Name",name,opts],dispName,opts]
reDef@
SetTensorName[t_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Name",name,opts]


def@
SetTensorDisplayName[t_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"DisplayName",name,opts]


def@
SetTensorValues[t_Tensor,values_List,opts:OptionsPattern[]]:=(ClearCachedTensorValues[t];SetTensorKeyValue[t,"Values",values,opts])
reDef@
SetTensorValues[t_Tensor/;Rank[t]==={0,0},values_,opts:OptionsPattern[]]:=(ClearCachedTensorValues[t];SetTensorKeyValue[t,"Values",values,opts])


RawTensorValues[___]:=Undefined;
RawTensorValues[t_Tensor]:=If[#=!=Undefined,If[AutoNameQ[t]||Not[$CacheTensorValues],#,RawTensorValues[TensorName[t],IndexPositions[t]]=#],Undefined]&[(Association@@t)["Values"]]


TensorValues[___]:=Undefined;
TensorValues[t_Tensor]:=RawTensorValues[t];
(*Module[{vals},
	vals = RawTensorValues[t];
	If[OnCurveQ[t]||Curve[t]===Undefined,
		vals,
		vals /. CurveRules[Curve[t]]
	]
]*)


testDef@
ValidateIndices[inds_List,test_?BooleanQ]:=
Module[{indsUp,repeatedInds,toCov},

	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	indsUp=toCov[inds];
	repeatedInds=RepeatedIndices[inds];
	
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		If[test,Return[False],
			Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
			AbortVerbose[]
		]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		If[test,Return[False],
			Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],toCov[#[[1]]],##&[]]&/@repeatedInds];
			AbortVerbose[]
		]
	];
	If[test,True]
];
testDef@ValidateIndices[inds_List]:=ValidateIndices[inds,False]


testDef@
ValidateIndices[t_Tensor,{inds___},test_?BooleanQ]:=
Module[{posInds,indsUp,repeatedInds,tfList},

	posInds=PossibleIndices[t];
	indsUp={inds}/.-sym_Symbol:>sym;
	repeatedInds=Cases[{inds},#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	(*The commented code is for when we add unique indices with dollar signs*)
	(*If[Complement[Symbol[StringTrim[ToString[Unique[#]],"$"~~__]]&/@indsUp,posInds]=!={},
		Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];
		Abort[]
	];*)
	If[Length[indsUp]=!=Total[Rank[t]],
		Print["The tensor ", t, " expects " ,Total[Rank[t]], " indices, but ", Length[indsUp], If[Length[indsUp]===1," index was ", " indices were "],"given."];
		AbortVerbose[]
	];

	If[MultipleMetricsQ[t],
		tfList=MapThread[MemberQ[#1,#2]&,{posInds,indsUp}];
		If[DeleteDuplicates@tfList=!={True},
			MapThread[If[Not@#1,Print["Given index ", #2, " not found in List of PossibleIndices ", #3],Nothing]&,{tfList,indsUp,posInds}];
			AbortVerbose[];
		],
		If[Complement[indsUp,posInds]=!={},
			Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];
			AbortVerbose[]
		]
	];

	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		AbortVerbose[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],#[[1]]/.-sym_Symbol:>sym,##&[]]&/@repeatedInds];
		AbortVerbose[]
	];
	
	If[Length[{inds}]=!=1 && CurveQ[t],
		Print["The curve ", t, " can only have 1 index, but ", Length[{inds}]," were given."];
		AbortVerbose[]
	];
];
testDef@ValidateIndices[t_Tensor,{inds___}]:=ValidateIndices[t,{inds},False]


def@
RepeatedIndices[inds_]:=
Module[{toCov,indsUp},
	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	indsUp=toCov[inds];
	Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp])
]


End[];

EndPackage[];
