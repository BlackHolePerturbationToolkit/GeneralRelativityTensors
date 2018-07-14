(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`TensorDefinitions`"];


Tensor::usage="Tensor is a Head created with the command ToTensor.";
ToTensor::usage="ToTensor[n,m,vals,inds] returns a Tensor with indices \
inds and TensorName n. The metric m and values vals \
(given as a consistently sized List) are assigned.
ToTensor[n,m,vals] is equivalent, but assumes all contravariant indices.";
ToCurve::usage="ToCurve[n,m,vals,param] returns a Tensor Curve \
that exists on the metric m. The Curve has the name n, \
values vals, and the parameter param."
ToTensorOnCurve::usage="ToTensorOnCurve[t,c] returns the Tensor t evaluated on the Curve c.
ToTensorOnCurve[n,c,vals,inds] returns a Tensor with indices \
inds and TensorName n and values vals evaluated on the curve c.
ToTensorOnCurve[n,c,vals] is equivalent, but assumes all contravariant indices.";
ToTensorFieldOnCurve::usage="ToTensorFieldOnCurve[t,c] returns the Tensor t with associated Curve c. \
Internally t is still treated as a function of the manifold's coordinates.";
CurveParameter::usage="CurveParameter[t] returns the parmeter which parametrizes the Tensor \
t along the Curve. It returns Undefined if t is not on a Curve.";
Curve::usage="Curve[t] returns the curve that \
t is defined along.";
OnCurveQ::usage="OnCurveQ[t] returns True if the values of Tensor t are evaluatated along on a Curve. \
OnCurveQ[t] also returns True if t is a Curve";
CurveQ::usage="CurveQ[t] returns True if the Tensor t is a Curve."
TensorFieldQ::usage="TensorFieldQ[t] returns True if \
the values of Tensor t are functions of the manifold's coordinates.";
CurveRules::usage="CurveRules[c] returns a list of rules sending the coordinates \
of a Curve c to its values."

ToMetric::usage="ToMetric[n,coords,vals,posInds] returns a metric Tensor with TensorName \
n, Coordinates coords, TensorValues \
vals, and PossibleIndices posInds.
ToMetric[builtIn] returns a built-in metric Tensor, where builtIn is a String such as \"Schwarzschild\".";
InverseMetric::usage="InverseMetric[t] returns the inverse metric Tensor associated with the Tensor \
t, or Undefined if no metric was set. If t is on a curve, InverseMetric[t] returns \
the inverse metric Tensor on the same curve.";
Metric::usage="Metric[expr] returns the metric Tensor associated with the Tensor expression expr.";
MetricQ::usage="MetricQ[t] returns True if the Tensor t is a metric.";

Coordinates::usage="Coordinates[expr] returns a List of symbols used for the coordinates of the Tensor \
expression expr.";
Rank::usage="Rank[t] returns the Tensor rank of the Tensor t as a List {p,q}, \
where p is the number of contravariant indices and q the number of covariant indices.";
Indices::usage="Indices[t] returns a List of Symbols representing the indices of the Tensor t. \
Positive Symbols are contravariant and negative Symbols are covariant.
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
SetTensorName::usage="SetTensorName[t,n] returns the Tensor t \
with its TensorName changed to n.";

RawTensorValues::usage="RawTensorValues[n,inds] returns the cached values of a Tensor \
with TensorName n and indices in positions inds or \
Undefined if none have been computed. The List inds should contain elements \"Up\" and/or \"Down\".
RawTensorValues[t] is equivalent to RawTensorValues[TensorName[t],\
IndexPositions[t]].";
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
SetRawTensorValues::usage="SetRawTensorValues[t,vals] returns the Tensor t with its RawTensorValues set to vals.";
ActOnTensorValues::usage="ActOnTensorValues[f,t] acts with the functions f on the values of Tensor t and returns the resulting tensor.";

AbstractQ::usage="AbstractQ[t] returns True if the Tensor t is treated as Abstract.";

ValidTensorExpressionQ::usage="ValidTensorExpressionQ[expr] tests whether a Tensor expression is valid are returns True if it is and False otherwise.";
ValidateTensorExpression::usage="ValidateTensorExpression[expr] checks whether a Tensor expression is valid and prints an error message and \
aborts if it is not.";


Begin["`Private`"];


$CacheTensorValues=False;


Tensor/:Format[t_Tensor]:=formatTensor[TensorDisplayName@t,Indices@t,CurveParameter@t]


Clear[formatTensor]
formatTensor[name_,inds_,param_]:=
Module[{upStr,dnStr,out1},
	out1=If[inds==={},
		name,
		dnStr=StringJoin[If[MatchQ[#,-_Symbol],ToString[#/.-x_:>x],"  "]&/@inds];
		upStr=StringJoin[If[Not@MatchQ[#,-_Symbol],ToString[#],"  "]&/@inds];
		SubsuperscriptBox[name,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
	];
	DisplayForm@If[param=!=Undefined,
		out1[ToString[param]],
		out1
	]
]


Tensor/:Coordinates[t_Tensor]:=(Association@@t)["Coordinates"]
Tensor/:Curve[t_Tensor]:=If[(Association@@t)["Curve"]==="Self",t,(Association@@t)["Curve"]]
Tensor/:Rank[t_Tensor]:=Module[{inds,co},inds=Indices[t];co=Count[inds,-_Symbol];{Length[inds]-co,co}];
Tensor/:AbstractQ[t_Tensor]:=(Association@@t)["Abstract"]
Tensor/:Dimensions[t_Tensor]:=(Association@@t)["Dimensions"]
Tensor/:Indices[t_Tensor]:=(Association@@t)["Indices"]
Tensor/:PossibleIndices[t_Tensor]:=(Association@@t)["PossibleIndices"]
Tensor/:CurveParameter[t_Tensor]:=(Association@@t)["CurveParameter"]
CurveQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["IsCurve"]
OnCurveQ[t_]:=MatchQ[t,_Tensor]&&(CurveParameter[t]=!=Undefined)
Tensor/:TensorName[t_Tensor]:=(Association@@t)["Name"]
Tensor/:TensorDisplayName[t_Tensor]:=(Association@@t)["DisplayName"]
Tensor/:IndexPositions[t_Tensor]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[t];
MetricQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["IsMetric"];


PossibleIndices[expr_]:=PossibleIndices[Metric[expr]]


Coordinates[expr_]:=Coordinates[Metric[expr]]


RawTensorValues[___]:=Undefined;
RawTensorValues[t_Tensor]:=If[#=!=Undefined,If[AutoNameQ[t]||Not[$CacheTensorValues],#,RawTensorValues[TensorName[t],IndexPositions[t]]=#],Undefined]&[(Association@@t)["Values"]]


TensorValues[___]:=Undefined;
TensorValues[t_Tensor]:=
Module[{vals},
	vals = RawTensorValues[t];
	If[OnCurveQ[t]||Curve[t]===Undefined,
		vals,
		vals /. CurveRules[Curve[t]]
	]
]


Clear[CurveRules]
Tensor/:CurveRules[c1_Tensor?CurveQ]:=Thread[Coordinates@c1->RawTensorValues@c1]


Clear[ToTensor]
ToTensor[assoc_Association]:=
Module[{keys,nullKeys,listKeys,indexChoices},
	keys={"IsMetric","Metric","Coordinates","Name","DisplayName","Indices","Values",
			"Abstract","Dimensions","PossibleIndices","IsCurve","Curve","CurveParameter"};
	nullKeys={"Metric","Coordinates","Values","PossibleIndices","Dimensions"};
	listKeys={"Coordinates","PossibleIndices","Indices"};

	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the tensor formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the tensor formation: "<>ToString[Complement[Keys[assoc],keys]]];
		Abort[]
	];
	If[Not@MatchQ[assoc["Indices"]/.-sym_Symbol:>sym,{___Symbol}],Print["Indices must be a list of Symbols (and negative Symbols)"];Abort[]];
	If[Not@MatchQ[assoc["PossibleIndices"],{___Symbol}],Print["PossibleIndices must be a list of Symbols"];Abort[]];

	If[assoc["Indices"]=!={}&&assoc["PossibleIndices"]=!={}&&Intersection[assoc["Indices"]/.(-a_Symbol:>a),assoc["PossibleIndices"]]==={},
		Print["Given Indices ", assoc["Indices"]/.(-a_Symbol:>a), " are not found in List of with PossibleIndices ", assoc["PossibleIndices"]];
		Abort[];
	];

	indexChoices=If[assoc["PossibleIndices"]==={},assoc["Indices"],assoc["PossibleIndices"]];

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
	
	If[Not@MatchQ[assoc["CurveParameter"],_Symbol],
		Print["\"CurveParameter\" must be a Symbol."];
		Abort[]
	];

	If[Not@BooleanQ[assoc["IsCurve"]],
		Print["\"IsCurve\" must be True or False."];
		Abort[]
	];

	If[assoc["IsCurve"]&&(assoc["CurveParameter"]===Undefined),
		Print["Curves must be parametrized."];
		Abort[]
	];
	
	If[assoc["Curve"]=!=Undefined&&assoc["Curve"]=!="Self"&&Not@CurveQ[assoc["Curve"]],
		Print["Given Option \"Curve\" is not a Curve."];
		Abort[]
	];
	
	If[MemberQ[assoc["PossibleIndices"],assoc["CurveParameter"]],
		Print["\"CurveParameter\" cannot also be a possible index."];
		Abort[]
	];
	
	If[assoc["CurveParameter"]=!=Undefined,
		checkForParam[assoc["Values"],assoc["Coordinates"],assoc["CurveParameter"]];
	];

	If[#=!=Undefined&&Not[AutoNameQ[assoc["Name"]]]&&$CacheTensorValues,RawTensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];
	Tensor@@(Normal@assoc/.("PossibleIndices"->_):>("PossibleIndices"->indexChoices))
]


ToTensor[{name_String,dispName_String},metric_Tensor?MetricQ,vals_,indsGiven_:Undefined]:=
Module[{coords,posInds,dims,inds,nInds},

	If[AbstractQ[metric],Print["Tensor with values cannot be defined using \"Abstract\" metric."];Abort[]];

	coords=Coordinates[metric];	
	posInds=PossibleIndices[metric];
	dims=Dimensions[metric];
	nInds=If[MatchQ[vals,_List],Length@Dimensions[vals],0];
	inds=If[indsGiven===Undefined,Take[posInds,nInds],indsGiven];
	ToTensor[Association["Coordinates"->coords,
						"Metric"->metric,
						"IsMetric"->False,
						"Name"->name,
						"DisplayName"->dispName,
						"Indices"->inds,
						"PossibleIndices"->posInds,
						"Abstract"->False,
						"Values"->vals,
						"Dimensions"->dims,
						"CurveParameter"->Undefined,
						"Curve"->Undefined,
						"IsCurve"->False]]
]
ToTensor[name_String,metric_Tensor?MetricQ,vals_List,indsGiven_:Undefined]:=ToTensor[{name,name},metric,vals,indsGiven];


Clear[builtInIndices]
builtInIndices[label_]:=
Switch[label,
		"Latin",
		Symbol/@CharacterRange["a","z"],
		"CapitalLatin",
		Symbol/@Complement[CharacterRange["A","Z"],{"D","C","E","I","K","N","O"}],
		"Greek",
		Symbol/@Complement[CharacterRange["\[Alpha]","\[Omega]"],{"\[Pi]","\[Tau]","\[Chi]"}],
		___,
		Print["No built-in indices ", label, ". Options are \"Latin\", \"CapitalLatin\", and \"Greek\""]; Abort[]
]


Clear[ToMetric]
ToMetric[assoc_Association]:=
Module[{keys,dims,posInds,inds},
	
	keys={"Coordinates","Name","Indices","Values","Abstract","PossibleIndices","DisplayName","CurveParameter","Curve","IsCurve"};
	
	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the metric tensor formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the metric tensor formation: "<>ToString[Complement[Keys[assoc],keys]]];
		Abort[]
	];
	posInds=Complement[If[MemberQ[{"Greek","Latin","CapitalLatin"},assoc["PossibleIndices"]],builtInIndices[assoc["PossibleIndices"]],assoc["PossibleIndices"]],
						Union[If[assoc["Coordinates"]=!=Undefined,assoc["Coordinates"],##&[]],Cases[assoc["Values"],_Symbol,Infinity]]];
	If[Length[posInds]<8,Print["At least 8 possible indices needed when defining a non-abstract metric"]];
	inds=If[assoc["Indices"]===Undefined,-Take[posInds,2],assoc["Indices"]];

	If[Not@MatchQ[inds,{-_Symbol,-_Symbol}]||(inds[[1]]===inds[[2]]),Print["Metric indices must be a pair of distinct covariant symbols"];Abort[]];
	If[assoc["Values"]=!=Undefined&&(Not@MatchQ[assoc["Values"],{Repeated[{__}]}]||Dimensions[assoc["Values"]]=!={Length@assoc["Coordinates"],Length@assoc["Coordinates"]}),
		Print["To be consistent with given coordinates, metric values must be given as a ",Length@assoc["Coordinates"], " \[Times] ", Length@assoc["Coordinates"], " matrix."];
		Abort[]
	];

	dims=If[assoc["Coordinates"]=!=Undefined,Length@assoc["Coordinates"],assoc["Coordinates"]];
	ToTensor[Join[KeyDrop[assoc,{"PossibleIndices","Indices"}],
					Association["Metric"->"Self",
								"IsMetric"->True,
								"Dimensions"->dims,
								"PossibleIndices"->posInds,
								"Indices"->inds,
								"CurveParameter"->Undefined,
								"Curve"->Undefined,
								"IsCurve"->False]]]
]


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
					"Values"->vals,
					"CurveParameter"->Undefined,
					"Curve"->Undefined,
					"IsCurve"->False]
		]
]
ToMetric[{name_String,dispName_String},coords_,vals_]:=ToMetric[{name,dispName},coords,vals,"Greek"]
ToMetric[name_String,coords_,vals_,posIndsParam_]:=ToMetric[{name,name},coords,vals,posIndsParam]
ToMetric[name_String,coords_,vals_]:=ToMetric[{name,name},coords,vals,"Greek"]


Clear[ToCurve]
ToCurve[{name_String,dispName_String},metric_Tensor?MetricQ,vals_List,param_Symbol]:=
Module[{posInds,coords,dims},

	posInds=PossibleIndices[metric];
	coords=Coordinates[metric];
	dims=Dimensions[metric];	
	If[dims=!=Length@vals,Print["Number of given values do not match metric dimensions"]; Abort[]];
	
	checkForParam[vals,coords,param];

	ToTensor[Association["Coordinates"->coords,
					"Metric"->metric,
					"IsMetric"->False,
					"Name"->name,
					"DisplayName"->dispName,
					"Indices"->{First@posInds},
					"PossibleIndices"->posInds,
					"Abstract"->False,
					"Values"->vals,
					"Dimensions"->dims,
					"IsCurve"->True,
					"Curve"->"Self",
					"CurveParameter"->param]]
]
ToCurve[name_String,metric_Tensor?MetricQ,vals_List,param_Symbol]:=ToCurve[{name,name},metric,vals,param];


Clear[checkForParam]
checkForParam[expr_,coords_,param_]:=
Module[{coordsP,temp,coordsPTemp,coordsPRules,exprTemp,notCurves},
	coordsP=Through[coords[param]];
	coordsPTemp=temp[ToString@#]& /@coords;
	coordsPRules=Thread[coordsP->coordsPTemp];
	exprTemp=expr/.coordsPRules;
	notCurves=Cases[{exprTemp},Alternatives@@coords,Infinity];
	If[notCurves=!={},
		Print["The following coordinates are given without being parametrized by the curve parameter: ",notCurves ];
		Abort[]
	];
]


Tensor/:Metric[t_Tensor]:=Which[(Association@@t)["Metric"]==="Self",
								t,
								Curve@t=!=Undefined,
								ToTensorFieldOnCurve[(Association@@t)["Metric"],Curve@t],
								True,
								(Association@@t)["Metric"]
							];


Metric[expr_]:=
Module[{metrics,metricNames},

	metrics=Cases[{expr},m_Tensor/;MetricQ[m],Infinity];
	If[metrics==={},Print["Expression ",expr, " does not contain a metric."] ;Abort[]];
	metricNames=DeleteDuplicates[TensorName/@metrics];
	If[Not[SameQ@@metricNames],Print["Expression contains Tensors with different metrics: ",metricNames ]; Abort[]];

	First@metrics
]


Clear[InverseMetric]
Tensor/:InverseMetric[t_Tensor]:=If[Metric@t===Undefined,Undefined,InverseMetric@Metric@t];
Tensor/:InverseMetric[t_Tensor?MetricQ]:=InverseMetric[t]=
Module[{assoc,tvStored,tv,posUp},

	posUp={"Up","Up"};
	tvStored=RawTensorValues[TensorName[t],posUp];
	tv=If[tvStored===Undefined,
			If[TensorValues[t]===Undefined,
				Undefined,
				Simplify@Inverse[RawTensorValues[Metric[t]]]
			],
			tvStored
		];
	
	assoc=Association@@t;
	ToTensor[Join[KeyDrop[assoc,{"Indices","Metric"}],
					Association["Indices"->Indices[t]/.-sym_Symbol:>sym,
								"Values"->tv,
								"Metric"->Metric[t]]]]
]


Tensor/:ActOnTensorValues[fn_,t_Tensor]:=SetRawTensorValues[t,Map[fn,RawTensorValues[t],{Total@Rank[t]}]]


Clear[ClearCachedTensorValues]
ClearCachedTensorValues[s_String,inds_]:=If[RawTensorValues[s,inds]=!=Undefined,Unset[RawTensorValues[s,inds]]]
ClearCachedTensorValues[str_String]:=Scan[ClearCachedTensorValues@@#&,Cases[CachedTensorValues[All],HoldPattern[{str,a___}->_]:>{str,a},Infinity]]
ClearCachedTensorValues[t_Tensor]:=Scan[ClearCachedTensorValues[TensorName[t],#]&,Tuples[{"Up","Down"},Total[Rank[t]]]]
ClearCachedTensorValues[All]:=Scan[ClearCachedTensorValues[Sequence@@#]&,
									DeleteDuplicates@Cases[DownValues[RawTensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[RawTensorValues][x__]]:>{x},{_String,{___String}}]]


Clear[CachedTensorValues]
CachedTensorValues[s_String]:=#->RawTensorValues@@#&/@(Cases[DownValues[RawTensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[RawTensorValues][x__]]:>{x},{s,{___String}}])
CachedTensorValues[t_Tensor]:=CachedTensorValues[TensorName[t]]
CachedTensorValues[All]:=CachedTensorValues/@DeleteDuplicates@Cases[DownValues[RawTensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[RawTensorValues][x__]]:>{x},{n_String,{___String}}:>n]


AutoNameQ[t_Tensor]:=AutoNameQ[TensorName[t]]
AutoNameQ[s_String]:=StringMatchQ[s,__~~"-Auto"]


Clear[SetTensorKeyValue]
Tensor/:SetTensorKeyValue[t_Tensor,key_String,value_]:=ToTensor[Join[KeyDrop[(Association@@t),{key}],Association[key->value]]]


Clear[SetMetric]
Tensor/:SetMetric[t_Tensor,m_Tensor]:=If[t=!=m,SetTensorKeyValue[t,"Metric",m],t]


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


Clear[SetAsAbstract]
Tensor/:SetAsAbstract[t_Tensor,tf_?BooleanQ]:=SetTensorKeyValue[t,"Abstract",tf]


Clear[SetRawTensorValues]
Tensor/:SetRawTensorValues[t_Tensor,values_List]:=SetTensorKeyValue[t,"Values",values]
Tensor/:SetRawTensorValues[t_Tensor,values_]/;Rank[t]==={0,0}:=SetTensorKeyValue[t,"Values",values]


Clear[ToTensorFieldOnCurve]
Tensor/:ToTensorFieldOnCurve[t1_Tensor,c1_?CurveQ]:=
Module[{params,paramVals},
	
	If[TensorName@Metric@t1=!=TensorName[(Association@@c1)["Metric"]],
		Print["Cannot put Tensor on a curve with a different metric."];
		Abort[]
	];
	
	params = {t1,{"Curve",c1}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]


Clear[ToTensorOnCurve]
Tensor/:ToTensorOnCurve[t1_Tensor,c1_?CurveQ]:=
Module[{params,vals},
	
	If[TensorName@Metric@t1=!=TensorName[(Association@@c1)["Metric"]],
		Print["Cannot put Tensor on a curve with a different metric."];
		Abort[]
	];
	
	vals=TensorValues@ToTensorFieldOnCurve[t1,c1];
	params = {t1,{"Values",vals},{"Curve",c1},{"CurveParameter",CurveParameter@c1}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]


ToTensorOnCurve[{name_String,displayName_String},c1_?CurveQ,vals_List,inds_:Undefined]/;MatchQ[inds,_List|Undefined]:=
Module[{t1,params},
	t1=ToTensor[{name,displayName},Metric[c1],vals,inds];
	params = {t1,{"Curve",c1},{"CurveParameter",CurveParameter@c1}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]
ToTensorOnCurve[name_String,c1_?CurveQ,vals_List,inds_:Undefined]/;MatchQ[inds,_List|Undefined]:=ToTensorOnCurve[{name,name},c1,vals,inds]


Indices[expr_]:=
Module[{terms,indicesList,tfList,sumQ,exprExpand},
	exprExpand=Expand[expr];
	terms=tensorExprTerms[exprExpand];
	indicesList=indicesInProduct/@terms;
	tfList=validateIndices[#,True]&/@indicesList;
	sumQ=SameQ@@(Sort/@indicesList);
	If[DeleteDuplicates[tfList]=!={True},
		MapThread[If[Not@#1,Print["The expression ",#2," has invalid indices."],Nothing]&,{tfList,terms}];
		Abort[]
	];
	If[Not@sumQ,Print["The expression does not have unique indices. Call IndicesTraced to get a unique list."];Abort[]];
	First[Sort/@indicesList]
]


IndicesTraced[expr_]:=
Module[{terms,indicesList,tfList,sumQ,exprExpand},
	exprExpand=Expand[expr];
	terms=tensorExprTerms[exprExpand];
	indicesList=indicesInProduct/@terms;
	tfList=validateIndices[#,True]&/@indicesList;
	sumQ=SameQ@@(Sort/@deleteRepeatedIndices/@indicesList);
	If[DeleteDuplicates[tfList]=!={True},
		MapThread[If[Not@#1,Print["The expression ",#2," has invalid indices."],Nothing]&,{tfList,terms}];
		Abort[]
	];
	If[Not@sumQ,Print["Cannot add Tensors with different indices."];Abort[]];
	First[Sort/@deleteRepeatedIndices/@indicesList]
]


Clear[ValidTensorExpressionQ]
ValidTensorExpressionQ[expr_]:=ValidateTensorExpression[expr,True]


Clear[ValidateTensorExpression]
ValidateTensorExpression[expr_,test_?BooleanQ]:=
Module[{exprExpand,tfList,terms,indicesList,sumQ,metricQ},
	exprExpand=Expand[expr];
	terms=tensorExprTerms[exprExpand,test];
	If[test&&Not@terms,Return@False];
	indicesList=indicesInProduct[#,test]&/@terms;
	If[test&&Cases[indicesList,False]=!={},Return@False];
	tfList=validateIndices[#,True]&/@indicesList;
	
	sumQ=SameQ@@(Sort/@deleteRepeatedIndices/@indicesList);
		
	metricQ=SameQ@@DeleteDuplicates@Cases[exprExpand,m_Tensor/;MetricQ[m]:>TensorName[m],Infinity];

	If[Not@test,
		If[DeleteDuplicates[tfList]=!={True},
			MapThread[If[Not@#1,Print["The expression ",#2," has invalid indices."],Nothing]&,{tfList,terms}];
			Abort[]
		];
		If[Not@sumQ,Print["Cannot add Tensors with different indices."];Abort[]];
		If[Not@metricQ,Print["Cannot combine Tensors with different metrics."]; Abort[]],
		DeleteDuplicates[tfList]==={True}&&sumQ&&metricQ
	]
]
ValidateTensorExpression[expr_]:=ValidateTensorExpression[expr,False];


Clear[tensorExprTerms]
tensorExprTerms[expr_,test_?BooleanQ]:=
Module[{exprExpand},
	exprExpand=Expand[expr];
	Which[MatchQ[exprExpand,_Plus],List@@exprExpand,
		MatchQ[exprExpand,_Tensor]||MatchQ[exprExpand,_Times],{exprExpand},
		True,If[test,False,Print["Expression should be a Tensor, a Tensor product, or a sum of Tensor (products), but is ", exprExpand]; Abort[]]
	]
];
tensorExprTerms[expr_]:=tensorExprTerms[expr,False];


Clear[indicesInProduct]
indicesInProduct[expr_,test_?BooleanQ]:=
Module[{exprExpand},
	exprExpand=Expand[expr];
	Which[
		MatchQ[exprExpand,Power[_Tensor,_]|__ Power[_Tensor,_]],If[test,False,Print["Tensors cannot be raised to a power as in ", exprExpand]; Abort[]],
		MatchQ[exprExpand,_Times],Join@@Cases[exprExpand,t_Tensor:>Indices[t],{1}],
		MatchQ[exprExpand,_Tensor],Indices[exprExpand],
		True,If[test,False,Print["Expression should be a Tensor or a Tensor product, but is ", exprExpand]; Abort[]]
	]
];
indicesInProduct[expr_]:=indicesInProduct[expr,False];


Clear[validateIndices]
validateIndices[inds_List,test_?BooleanQ]:=
Module[{indsUp,repeatedInds,toCov},

	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	indsUp=toCov[inds];
	repeatedInds=repeatedIndices[inds];
	
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		If[test,Return[False],
			Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
			Abort[]
		]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		If[test,Return[False],
			Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],toCov[#[[1]]],##&[]]&/@repeatedInds];
			Abort[]
		]
	];
	
	If[test,True]
];
validateIndices[inds_List]:=validateIndices[inds,False]


Clear[repeatedIndices]
repeatedIndices[inds_]:=
Module[{toCov,indsUp},
	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	indsUp=toCov[inds];
	Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp])
]


Clear[deleteRepeatedIndices]
deleteRepeatedIndices[inds_]:=DeleteCases[inds,Alternatives@@Flatten@repeatedIndices[inds]]


End[];

EndPackage[];
