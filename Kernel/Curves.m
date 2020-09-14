(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`Curves`",
			{"GeneralRelativityTensors`BasicTensors`",
			"GeneralRelativityTensors`Utils`"}];


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
Curve::usage="Curve[t] returns the Curve associated with Tensor t.";
OnCurveQ::usage="OnCurveQ[t] returns True if the values of Tensor t are evaluatated along on a Curve. \
OnCurveQ[t] also returns True if t is a Curve";
CurveQ::usage="CurveQ[t] returns True if the Tensor t is a Curve."

CurveRules::usage="CurveRules[c] returns a list of rules sending the coordinates \
of a Curve c to its values."


SetCurve::usage="SetCurve[t,c] returns the Tensor t with its Curve set to c.";
SetCurveQ::usage="SetCurveQ[t,bool] returns the Tensor t with its CurveQ flag set to bool (True or False).";
SetCurveParameter::usage="SetCurve[t,param] returns the Tensor t with its CurveParameter set to param.";


Begin["`Private`"];


Options[SetCurve]=Options[SetTensorKeyValue];
Options[SetCurveQ]=Options[SetTensorKeyValue];
Options[SetCurveParameter]=Options[SetTensorKeyValue];


DocumentationBuilder`OptionDescriptions["SetCurve"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCurveQ"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCurveParameter"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];


Tensor/:Format[t_Tensor?(CurveParameter[#]=!=Undefined &)]:=formatCurve[TensorDisplayName@t,Indices@t,CurveParameter@t]


Clear[indicesStrings]
indicesStrings[inds_]:=
Module[{dnStr,upStr},
	dnStr=StringJoin[If[MatchQ[#,-_Symbol],ToString[#/.-x_:>x],StringJoin@Table["  ",StringLength[ToString[#]]]]&/@inds];
	upStr=StringJoin[If[Not@MatchQ[#,-_Symbol],ToString[#],StringJoin@Table["  ",StringLength[ToString[#/.-x_:>x]]]]&/@inds];

	{upStr,dnStr}
]


Clear[formatTensorBoxes]
formatTensorBoxes[name_,inds_,param_]:=
Module[{upStr,dnStr,out1,nameStr,out},
	nameStr = If[MatchQ[name,_String],name,ToString[name]];
	out=If[inds==={},
		nameStr,
		{upStr,dnStr}=indicesStrings[inds];
		SubsuperscriptBox[nameStr,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
	];
	out[param]
]


Clear[formatCurve]
formatCurve[name_,inds_,param_]:=DisplayForm@formatTensorBoxes[name,inds,param]


def@Curve[t_Tensor]:=If[Not@MemberQ[Keys[Association@@t],"Curve"],Undefined,If[(Association@@t)["Curve"]==="Self",t,(Association@@t)["Curve"]]];


def@CurveParameter[t_Tensor]:=If[Not@MemberQ[Keys[Association@@t],"CurveParameter"],Undefined,(Association@@t)["CurveParameter"]];
def@CurveQ[t_]:=MatchQ[t,_Tensor]&&MemberQ[Keys[Association@@t],"CurveQ"]&&(Association@@t)["CurveQ"];
def@OnCurveQ[t_]:=MatchQ[t,_Tensor]&&(CurveParameter[t]=!=Undefined);


def@
CurveRules[c1_Tensor?CurveQ]:=Thread[Coordinates@c1->RawTensorValues@c1]


def@
ToCurve[{name_String,dispName_String},metric_Tensor?MetricQ,vals_List,param_Symbol]:=
Module[{posInds,coords,assoc},

	posInds=PossibleIndices[metric];
	coords=Coordinates[metric];
	(*checkForParam[vals,coords,param];*)
	assoc=KeySort@Association["Coordinates"->{coords},
								"Curve"->"Self",
								"CurveParameter"->param,
								"CurveQ"->True,
								"DisplayName"->dispName,
								"Indices"->{First@posInds},
								"Metric"->{metric},
								"MetricQ"->False,
								"Name"->name,
								"PossibleIndices"->{posInds},
								"Values"->vals];
	tensorCurveTests[assoc];

	ToTensor[assoc]
]
reDef@
ToCurve[name_String,metric_Tensor?MetricQ,vals_List,param_Symbol]:=ToCurve[{name,name},metric,vals,param];


testDef@
tensorCurveTests[assoc_Association]:=
Module[{keys},
	keys={"Curve","CurveParameter","CurveQ","PossibleIndices","Values"};

	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorCurveTests: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["CurveParameter"],_Symbol],
		Print["\"CurveParameter\" must be a Symbol or Undefined."];
		AbortVerbose[]
	];

	If[Not@BooleanQ[assoc["CurveQ"]],
		Print["\"CurveQ\" must be True or False."];
		AbortVerbose[]
	];

	If[assoc["CurveQ"]&&(assoc["CurveParameter"]===Undefined),
		Print["Curves must be parametrized."];
		AbortVerbose[]
	];
	
	If[assoc["Curve"]=!=Undefined&&assoc["Curve"]=!="Self"&&
		Not@CurveQ[assoc["Curve"]],
		Print["Given Option \"Curve\" is not a Curve."];
		AbortVerbose[]
	];
	
	If[MemberQ[assoc["PossibleIndices"],assoc["CurveParameter"]],
		Print["\"CurveParameter\" cannot also be a possible index."];
		AbortVerbose[]
	];
	
	If[assoc["CurveParameter"]=!=Undefined,
		checkForParam[assoc["Values"],assoc["Coordinates"],assoc["CurveParameter"]];
	];
]



def@
checkForParam[expr_,coords_List,param_]:=
Module[{coordsP,temp,coordsPTemp,coordsPRules,exprTemp,notCurves},
	coordsP=Through[coords[param]];
	coordsPTemp=temp[ToString@#]& /@coords;
	coordsPRules=Thread[coordsP->coordsPTemp];
	exprTemp=expr/.coordsPRules;
	notCurves=Complement[DeleteDuplicates@Cases[{exprTemp},(Alternatives@@coords),Infinity],{param}];

	If[notCurves=!={},
		Print["The following coordinates are given without being parametrized by the curve parameter: ",notCurves];
		AbortVerbose[]
	];
]


def@
SetCurve[t_Tensor,c_Tensor,opts:OptionsPattern[]]:=If[t=!=c,SetTensorKeyValue[t,"Curve",c],t,opts]


def@
SetCurveQ[t_Tensor,bool_?BooleanQ,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"CurveQ",bool,opts]


def@
SetCurveParameter[t_Tensor,param_Symbol,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"CurveParameter",param,opts]


def@
ToTensorFieldOnCurve[t1_Tensor,c1_?CurveQ]:=
Module[{params},
	
	If[TensorName@Metric@t1=!=TensorName@Metric@c1,
		Print["Cannot put Tensor on a curve with a different metric."];
		AbortVerbose[]
	];
	
	params = {t1,{"Curve",c1}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]


def@
ToTensorOnCurve[t1_Tensor,c1_?CurveQ]:=
Module[{params,vals},
	
	If[TensorName@Metric@t1=!=TensorName@Metric@c1,
		Print["Cannot put Tensor on a curve with a different metric."];
		AbortVerbose[]
	];
	vals=TensorValues@ToTensorFieldOnCurve[t1,c1];
	params = {t1,{"Name",TensorName[t1]<>"Curve"},
					{"Values",vals},
					{"Curve",c1},
					{"CurveParameter",CurveParameter@c1},
					{"MetricQ",False},
					{"Metric",Metric[t1,"PerIndex"->True]}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]


reDef@
ToTensorOnCurve[{name_String,displayName_String},c1_?CurveQ,vals_List,inds_:Undefined]:=
Module[{t1,params},
	t1=ToTensor[{name,displayName},Metric[c1],vals,inds];
	params = {t1,{"Curve",c1},{"CurveQ",False},{"CurveParameter",CurveParameter@c1}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]
reDef@
ToTensorOnCurve[name_String,c1_?CurveQ,vals_List,inds_:Undefined]:=ToTensorOnCurve[{name,name},c1,vals,inds]


TensorValues[t_Tensor?OnCurveQ]:=RawTensorValues[t]
TensorValues[t_Tensor?(Curve[#]=!=Undefined&)]:=RawTensorValues[t]/.CurveRules[Curve[t]]


End[];

EndPackage[];
