(* ::Package:: *)

BeginPackage["Tensors`TensorDefinitions`"];


TensorDefinitions`Private`str[a_String]:=ToString[Style[a,Italic],TraditionalForm]
TensorDefinitions`Private`str[args___]:=StringRiffle[TensorDefinitions`Private`str/@{args},","]


Tensor::usage="Tensor is a Head created with the command ToTensor.";
ToTensor::usage="ToTensor["<>TensorDefinitions`Private`str["n","{inds}"]<>"] returns a Tensor with TensorName "<>TensorDefinitions`Private`str["n"]<>" \
and indices "<>TensorDefinitions`Private`str["inds"]<>".
ToTensor["<>TensorDefinitions`Private`str["n","m","vals"]<>"] returns a contravariant Tensor with TensorName "<>TensorDefinitions`Private`str["n"]<>". \
The (non-abstract) metric "<>TensorDefinitions`Private`str["m"]<>" and values "<>TensorDefinitions`Private`str["vals"]<>" (given as a consistently sized List) are assigned.
ToTensor["<>TensorDefinitions`Private`str["n","m","vals","{inds}"]<>"] returns a Tensor with indices "<>TensorDefinitions`Private`str["inds"]<>" and TensorName \
"<>TensorDefinitions`Private`str["n"]<>". The (non-abstract) metric "<>TensorDefinitions`Private`str["m"]<>" and values "<>TensorDefinitions`Private`str["vals"]<>" \
(given as a consistently sized List) are assigned.";

ToCurve::usage="ToCurve["<>TensorDefinitions`Private`str["n","m","vals","param"]<>"] returns a Tensor curve \
that exists in the metric "<>TensorDefinitions`Private`str["m"]<>". The curve has the name "<>TensorDefinitions`Private`str["n"]<>", \
values "<>TensorDefinitions`Private`str["vals"]<>", and the parameter "<>TensorDefinitions`Private`str["param"]<>"."
TensorOnCurve::usage="TensorOnCurve["<>TensorDefinitions`Private`str["t","c"]<>"] returns the Tensor "<>TensorDefinitions`Private`str["t"]<>" \
evaluated on the curve "<>TensorDefinitions`Private`str["c"]<>"."
CurveParameter::usage="CurveParameter["<>TensorDefinitions`Private`str["t"]<>"] returns the parmeter which parametrizes the tensor \
"<>TensorDefinitions`Private`str["t"]<>" along the curve. It returns Undefined if "<>TensorDefinitions`Private`str["t"]<>" is not a curve.";
Curve::usage="Curve["<>TensorDefinitions`Private`str["t"]<>"] returns the curve that  \
"<>TensorDefinitions`Private`str["t"]<>" is defined along. It returns Undefined if "<>TensorDefinitions`Private`str["t"]<>" is not on a curve. \
Note that Curve["<>TensorDefinitions`Private`str["t"]<>"] will return "<>TensorDefinitions`Private`str["t"]<>" itself if it is a curve.";
OnCurveQ::usage="OnCurveQ["<>TensorDefinitions`Private`str["t"]<>"] returns True if the Tensor \
"<>TensorDefinitions`Private`str["t"]<>" is defined on a curve. OnCurveQ["<>TensorDefinitions`Private`str["t"]<>"] \
also returns True if "<>TensorDefinitions`Private`str["t"]<>" is a curve";
CurveQ::usage="CurveQ["<>TensorDefinitions`Private`str["t"]<>"] returns True if the Tensor \
"<>TensorDefinitions`Private`str["t"]<>" is a curve."
ParametrizedValuesQ::usage="ParametrizedValuesQ["<>TensorDefinitions`Private`str["t"]<>"] returns True if \
the values of Tensor "<>TensorDefinitions`Private`str["t"]<>" are parametrized on a curve."

ToMetric::usage="ToMetric["<>TensorDefinitions`Private`str["n","coords","vals","inds"]<>"] returns an non-abstract metric Tensor with TensorName \
"<>TensorDefinitions`Private`str["n"]<>", Coordinates "<>TensorDefinitions`Private`str["coords"]<>", TensorValues \
"<>TensorDefinitions`Private`str["vals"]<>", and PossibleIndices "<>TensorDefinitions`Private`str["inds"]<>" \
(where "<>TensorDefinitions`Private`str["inds"]<>" can be \"Greek\",\"Latin\",\"CaptialLatin\" or a List of Symbols).
ToMetric["<>TensorDefinitions`Private`str["builtIn"]<>"] returns a built-in metric Tensor, where "<>TensorDefinitions`Private`str["builtIn"]<>" \
can be \"Minkowski\" (or \"Mink\"), \"Schwarzschild\" (or \"Schw\"), \"Kerr\", \"ReissnerNordstrom\" \
(or \"RN\"), \"TwoSphere\" (or \"S2\"), \"SchwarzschildM2\" (or \"SchwM2\"), \"SchwarzschildS2\" (or \"SchwS2\"), \"ReissnerNordstromM2\" (or \"RNM2\"), \
or \"ReissnerNordstromS2\" (or \"RNS2\").";
InverseMetric::usage="InverseMetric["<>TensorDefinitions`Private`str["t"]<>"] returns the inverse metric Tensor associated with the Tensor \
"<>TensorDefinitions`Private`str["t"]<>", or Undefined if no metric was set. If "<>TensorDefinitions`Private`str["t"]<>" is on a curve, InverseMetric[t] returns \
the inverse metric Tensor on the same curve.";
Metric::usage="Metric["<>TensorDefinitions`Private`str["t"]<>"] returns the metric Tensor associated with the Tensor "<>TensorDefinitions`Private`str["t"]<>", or Undefined \
if no metric was set. Note that Metric["<>TensorDefinitions`Private`str["t"]<>"] will return "<>TensorDefinitions`Private`str["t"]<>" itself if it is a metric.\
If "<>TensorDefinitions`Private`str["t"]<>" is on a curve, Metric[t] returns the metric Tensor on the same curve.";
MetricQ::usage="MetricQ["<>TensorDefinitions`Private`str["t"]<>"] returns True if the Tensor \
"<>TensorDefinitions`Private`str["t"]<>" is a metric.";

Coordinates::usage="Coordinates["<>TensorDefinitions`Private`str["t"]<>"] returns a List of symbols used for the coordinates of the Tensor \
"<>TensorDefinitions`Private`str["t"]<>", or Undefined if coordinates were not set.";
Rank::usage="Rank["<>TensorDefinitions`Private`str["t"]<>"] returns the Tensor rank of the Tensor "<>TensorDefinitions`Private`str["t"]<>" as a List {p,q}, \
where p is the number of contravariant indices and q the number of covariant indices.";
Indices::usage="Indices["<>TensorDefinitions`Private`str["t"]<>"] returns a List of Symbols representing the indices of the Tensor "<>TensorDefinitions`Private`str["t"]<>". \
Positive Symbols are contravariant and negative Symbols are covariant.";
PossibleIndices::usage="PossibleIndices["<>TensorDefinitions`Private`str["t"]<>"] returns a List of all possible Symbols that can represent the indices of the \
Tensor "<>TensorDefinitions`Private`str["t"]<>".";
IndexPositions::usage="IndexPositions["<>TensorDefinitions`Private`str["t"]<>"] returns a List of elements \
\"Up\" and \"Down\" which represent (respectively) the contravariant and covariant positions of the \
indices of Tensor "<>TensorDefinitions`Private`str["t"]<>".";

TensorName::usage="TensorName["<>TensorDefinitions`Private`str["t"]<>"] returns the name of Tensor \
"<>TensorDefinitions`Private`str["t"]<>" which is used for storing cached values in the Symbol TensorValues.";
TensorDisplayName::usage="TensorDisplayName["<>TensorDefinitions`Private`str["t"]<>"] returns the name of \
Tensor "<>TensorDefinitions`Private`str["t"]<>" that is used for formatted output.";
SetTensorName::usage="SetTensorName["<>TensorDefinitions`Private`str["t","n"]<>"] returns the Tensor "<>TensorDefinitions`Private`str["t"]<>" \
with its TensorName changed to "<>TensorDefinitions`Private`str["n"]<>".";

RawTensorValues::usage="RawTensorValues["<>TensorDefinitions`Private`str["n","inds"]<>"] returns the cached values of a Tensor \
with TensorName "<>TensorDefinitions`Private`str["n"]<>" and indices in positions "<>TensorDefinitions`Private`str["inds"]<>" or \
Undefined if none have been computed. The List "<>TensorDefinitions`Private`str["inds"]<>" should contain elements \"Up\" and/or \"Down\".
RawTensorValues["<>TensorDefinitions`Private`str["t"]<>"] is equivalent to RawTensorValues[TensorName["<>TensorDefinitions`Private`str["t"]<>"],\
IndexPositions["<>TensorDefinitions`Private`str["t"]<>"]].";
TensorValues::usage="TensorValues["<>TensorDefinitions`Private`str["t"]<>"] returns the RawTensorValues of "<>TensorDefinitions`Private`str["t"]<>". \
If "<>TensorDefinitions`Private`str["t"]<>" has an associated curve, the values are evaluated along the curve.";
ClearCachedTensorValues::usage="ClearCachedTensorValues["<>TensorDefinitions`Private`str["n","inds"]<>"] removes cached expressions stored with \
the Symbol RawTensorValues using the TensorName "<>TensorDefinitions`Private`str["n"]<>" and IndexPositions "<>TensorDefinitions`Private`str["inds"]<>". \
Here inds is a List of \"Up\" and \"Down\".
ClearCachedTensorValues["<>TensorDefinitions`Private`str["n"]<>"] removes all cached expressions stored with the Symbol \
RawTensorValues for any Tensor with name "<>TensorDefinitions`Private`str["n"]<>".
ClearCachedTensorValues["<>TensorDefinitions`Private`str["t"]<>"] removes all cached expressions stored with the Symbol \
RawTensorValues for the Tensor "<>TensorDefinitions`Private`str["t"]<>".
ClearCachedTensorValues[All] removes all cached expressions associated with the Symbol RawTensorValues.";
CachedTensorValues::usage="CachedTensorValues["<>TensorDefinitions`Private`str["n"]<>"] returns a List of Rules showing all cached expressions \
for the TensorName "<>TensorDefinitions`Private`str["n"]<>" (stored in the Symbol RawTensorValues).
CachedTensorValues["<>TensorDefinitions`Private`str["t"]<>"] returns a List of Rules showing all cached expressions \
for the Tensor "<>TensorDefinitions`Private`str["t"]<>" (stored in the Symbol RawTensorValues).
CachedTensorValues[All] returns a List of Rules showing all cached expressions (stored in the Symbol RawTensorValues)."
$CacheTensorValues::usage="$CacheTensorValues is a global boolean (with default value True) specifying whether to cache Tensor values in the symbol RawTensorValues."
SetRawTensorValues;
ActOnTensorValues;

AbstractQ::usage="AbstractQ["<>TensorDefinitions`Private`str["t"]<>"] returns True if the Tensor \
"<>TensorDefinitions`Private`str["t"]<>" is treated as Abstract.";


Begin["`Private`"];


$CacheTensorValues=True;


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
ParametrizedValuesQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["ParametrizedValues"]
OnCurveQ[t_]:=MatchQ[t,_Tensor]&&((Association@@t)["Curve"]=!=Undefined)
Tensor/:TensorName[t_Tensor]:=(Association@@t)["Name"]
Tensor/:TensorDisplayName[t_Tensor]:=(Association@@t)["DisplayName"]
Tensor/:IndexPositions[t_Tensor]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[t];
MetricQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["IsMetric"];


RawTensorValues[___]:=Undefined;
RawTensorValues[t_Tensor]:=If[#=!=Undefined,If[AutoNameQ[t]||Not[$CacheTensorValues],#,RawTensorValues[TensorName[t],IndexPositions[t]]=#],Undefined]&[(Association@@t)["Values"]]


TensorValues[___]:=Undefined;
TensorValues[t_Tensor]:=
Module[{vals},
	vals = RawTensorValues[t];
	If[OnCurveQ[t]&&Not@CurveQ@t&&Not@ParametrizedValuesQ[t],
		vals /. Thread[Coordinates@Curve[t]->RawTensorValues@Curve[t]],
		vals
	]
]


Clear[ToTensor]
ToTensor[assoc_Association]:=
Module[{keys,nullKeys,listKeys,indexChoices},
	keys={"IsMetric","Metric","Coordinates","Name","DisplayName","Indices","Values","ParametrizedValues",
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

	If[Not@BooleanQ[assoc["ParametrizedValues"]],
		Print["\"ParametrizedValues\" must be True or False."];
		Abort[]
	];

	If[assoc["IsCurve"]&&(Not@assoc["ParametrizedValues"]||assoc["CurveParameter"]===Undefined),
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
	
	If[assoc["ParametrizedValues"],
		checkForParam[assoc["Values"],assoc["Coordinates"],assoc["CurveParameter"]];
	];

	If[#=!=Undefined&&Not[AutoNameQ[assoc["Name"]]]&&$CacheTensorValues,RawTensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];
	Tensor@@(Normal@assoc/.("PossibleIndices"->_):>("PossibleIndices"->indexChoices))
]


Options[ToTensor]={"Coordinates"->Undefined,
					"DisplayName"->Undefined,
					"Metric"->Undefined,
					"IsMetric"->False,
					"PossibleIndices"->{},
					"Abstract"->True,
					"Values"->Undefined,
					"Dimensions"->Undefined,
					"CurveParameter"->Undefined,
					"ParametrizedValues"->False,
					"Curve"->Undefined,
					"IsCurve"->False};
ToTensor[{name_String,dispName_String},{inds___},opts:OptionsPattern[]]:=
Module[{coords,vals,posInds,abstr,metric,dims,isMetric,param,curve,isCurve,pVals},
	coords=OptionValue["Coordinates"];
	vals=OptionValue["Values"];
	posInds=OptionValue["PossibleIndices"];
	abstr=OptionValue["Abstract"];
	metric=OptionValue["Metric"];
	isMetric=OptionValue["IsMetric"];
	dims=OptionValue["Dimensions"];
	param=OptionValue["CurveParameter"];
	pVals=OptionValue["ParametrizedValues"];
	curve=OptionValue["Curve"];
	isCurve=OptionValue["IsCurve"];
	
	If[MetricQ[metric],
		If[posInds==={},posInds=PossibleIndices[metric]];
		If[dims===Undefined,dims=Dimensions[metric],If[dims=!=Dimensions[metric],Print["Given dimensions do not match metric dimensions"];Abort[]]];
		If[coords===Undefined,coords=Coordinates[metric],If[coords=!=Coordinates[metric],Print["Given coordinates do not match metric coordinates"];Abort[]]];
		If[vals=!=Undefined,abstr=False];
	];
	
	ToTensor[Association["Coordinates"->coords,
		"Metric"->metric,
		"IsMetric"->isMetric,
		"Name"->name,
		"DisplayName"->dispName,
		"Indices"->{inds},
		"PossibleIndices"->posInds,
		"Abstract"->abstr,
		"Values"->vals,
		"Dimensions"->dims,
		"CurveParameter"->param,
		"ParametrizedValues"->pVals,
		"Curve"->curve,
		"IsCurve"->isCurve]]
]
ToTensor[name_String,{inds___},opts:OptionsPattern[]]:=ToTensor[{name,name},{inds},opts]


ToTensor[{name_String,dispName_String},metric_Tensor?MetricQ,vals_List,indsGiven_:Undefined]:=
Module[{coords,posInds,dims,inds},

	If[AbstractQ[metric],Print["Tensor with values cannot be defined using \"Abstract\" metric."];Abort[]];

	coords=Coordinates[metric];	
	posInds=PossibleIndices[metric];
	dims=Dimensions[metric];
	inds=If[indsGiven===Undefined,Take[posInds,Length@Dimensions[vals]],indsGiven];
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
						"ParametrizedValues"->False,
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
	
	keys={"Coordinates","Name","Indices","Values","Abstract","PossibleIndices","DisplayName","ParametrizedValues","CurveParameter","Curve","IsCurve"};
	
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
								"ParametrizedValues"->False,
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
					"ParametrizedValues"->False,
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
					"CurveParameter"->param,
					"ParametrizedValues"->True]]
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
								ParametrizedValuesQ@t,
								TensorOnCurve[(Association@@t)["Metric"],Curve@t],
								True,
								(Association@@t)["Metric"]
							];


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


Tensor/:ActOnTensorValues[t_Tensor,fn_]:=SetRawTensorValues[t,fn@RawTensorValues[t]]


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


Clear[TensorOnCurve]
Options[TensorOnCurve]={"ParametrizedValues"->False};
TensorOnCurve[t1_Tensor,c1_?CurveQ,opts:OptionsPattern[]]:=
Module[{params,paramVals},
	paramVals = OptionValue["ParametrizedValues"];
	If[Not@BooleanQ@paramVals,
		Print["\"ParametrizedValues\" must be a boolean"];
		Abort[]
	];
	params = {t1,{"Curve",c1},{"CurveParameter",CurveParameter@c1},{"ParametrizedValues",paramVals}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
	(*SetTensorKeyValue[
		SetTensorKeyValue[
			SetTensorKeyValue[t1,"Curve",c1],"CurveParameter",CurveParameter@c1],"ParametrizedValues",paramVals]*)
]


End[];

EndPackage[];
