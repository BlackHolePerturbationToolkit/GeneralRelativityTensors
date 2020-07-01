(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`TensorDefinitions`",{"GeneralRelativityTensors`Utils`"}];


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
Curve::usage="Curve[t] returns the Curve associated with Tensor t.";
OnCurveQ::usage="OnCurveQ[t] returns True if the values of Tensor t are evaluatated along on a Curve. \
OnCurveQ[t] also returns True if t is a Curve";
CurveQ::usage="CurveQ[t] returns True if the Tensor t is a Curve."
TensorFieldQ::usage="TensorFieldQ[t] returns True if \
the values of Tensor t are functions of the manifold's coordinates.";
CurveRules::usage="CurveRules[c] returns a list of rules sending the coordinates \
of a Curve c to its values."

ToTensorWithTetrad::usage="ToTensorWithTetrad[t,tet] returns the Tensor t with its Tetrad set to tet \
and its values set to its tetrad components.";

ToMetric::usage="ToMetric[n,coords,vals,posInds] returns a metric Tensor with TensorName \
n, Coordinates coords, TensorValues vals, and PossibleIndices posInds.
ToMetric[builtIn] returns a built-in metric Tensor, where builtIn is a String such as \"Schwarzschild\".";
InverseMetric::usage="InverseMetric[t] returns the inverse metric Tensor associated with the Tensor \
t, or Undefined if no metric was set. If t is on a curve, InverseMetric[t] returns \
the inverse metric Tensor on the same curve.";
Metric::usage="Metric[expr] returns the metric Tensor associated with the Tensor expression expr.";
MetricQ::usage="MetricQ[t] returns True if the Tensor t is a metric.";

SpacetimeDimensions::usage="SpacetimeDimensions[t] returns the number of spacetime dimensions in the \
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
SetSpacetimeDimensions::usage="SetSpacetimeDimensions[t,dims] returns the Tensor t with its SpacetimeDimensions set to dims.";
SetCoordinates::usage="SetCoordinates[t,coords] returns the Tensor t with its Coordinates set to coords.";
SetIndices::usage="SetIndices[t,inds] returns the Tensor t with its Indices set to inds.";
SetPossibleIndices::usage="SetPossibleIndices[t,posInds] returns the Tensor t with its PossibleIndices set to posInds.";
SetMetric::usage="SetMetric[t,m] returns the Tensor t with its Metric set to m.";
SetMetricQ::usage="SetMetricQ[t,bool] returns the Tensor t with its MetricQ flag set to bool (True or False).";
SetCurve::usage="SetCurve[t,c] returns the Tensor t with its Curve set to c.";
SetCurveQ::usage="SetCurveQ[t,bool] returns the Tensor t with its CurveQ flag set to bool (True or False).";
SetCurveParameter::usage="SetCurve[t,param] returns the Tensor t with its CurveParameter set to param.";
SetAbstractQ::usage="SetAbstractQ[t,bool] returns the Tensor t with its AbstractQ flag set to bool (True or False).";
SetTetrad::usage="SetTetrad[t,tet] returns the Tensor t with its Tetrad set to tet.";

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

AbstractQ::usage="AbstractQ[t] returns True if the Tensor t is treated as Abstract.";

ValidTensorExpressionQ::usage="ValidTensorExpressionQ[te] tests whether a TensorExpression is valid and returns True if it is and False otherwise.";
ValidateTensorExpression::usage="ValidateTensorExpression[te] checks whether a TensorExpression is valid and prints an error message and \
aborts if it is not.";

SubmanifoldsQ::usage="SubmanifoldsQ[t] returns True if Tensor t is defined with Submanifolds.";
MultipleMetricsQ::usage="MultipleMetricsQ[t] returns True if the indices of Tensor t are raised and lowered with different metrics.";
(*SubmanifoldForm::usage="SubmanifoldForm[t] returns the values of Tensor t split into nested Lists corresponding to its submanifolds.";*)
Submetrics::usage="Submetrics[t] returns the List of Metrics for the submanifolds of Tensor t.";

ValidateIndices;
RepeatedIndices;

SetTensorExpressionTerms;
SetTensorExpressionIndices;
TensorExpressionDisplayName;
SetTensorExpressionDisplayName;
SetTensorExpressionMetric;

TensorPattern::usage="TensorPattern[t,patInds] returns the Tensor t but with its \
indices replaced by patInds, a List of patterns.
TensorPattern[_,patInds] returns Tensor with its \
indices replaced by patInds, a List of patterns, and all other values replaced by Blank[].";



Tetrad::usage="Tetrad is a Head created with the command ToTetrad.";
ToTetrad::usage="ToTetrad[n,vecs,posInds] returns a Tetrad with name n made of the four vectors vecs with \
possible TetradIndices posInds.";
TetradValues::usage="TetradValues[tet] returns a 4x4 matrix of values corresponding to the Tetrad tet.";
TetradVectors::usage="TetradVectors[tet] returns the four vectors (or co-vectors) stored by the Tetrad tet.";
SpacetimeIndex::usage="SpacetimeIndex[tet] returns the second index of the Tetrad tet.";
TetradIndex::usage="TetradIndex[tet] returns the first index of the Tetrad tet.";
PossibleSpacetimeIndices::usage="PossibleSpacetimeIndices[tet] returns the list of PossibleIndices that can be \
used in the second Index of the Tetrad tet.";
PossibleTetradIndices::usage="PossibleTetradIndices[tet] returns the list of PossibleIndices that can be \
used in the first Index of the Tetrad tet.";
TetradName::usage="TetradName[tet] returns the name of Tetrad tet.";
TetradDisplayName::usage="TetradDisplayName[tet] returns the name of Tetrad tet that is used for formatted output.";
SpacetimeMetric::usage="SpacetimeMetric[tet] returns the metric used by the Vectors of the Tetrad tet.";
TetradMetric::usage="TetradMetric[tet] returns the tetrad formed from the vectors of the Tetrad tet.";


SetTetradKeyValue::usage="SetTetradKeyValue[tet,key,value] returns the Tetrad tet with the appropriate Rule changed to key->value.";
SetTetradName::usage="SetTetradName[tet,n] returns the Tetrad tet with its TetradName changed to n.";
SetTetradDisplayName::usage="SetTetradDisplayName[tet,n] returns the Tetrad tet with its TetradDisplayName changed to n.";
SetTetradVectors::usage="SetTetradVectors[tet,vecs] returns the Tetrad tet with its Vectors set to vecs.";
SetTetradIndex::usage="SetTetradIndex[tet,ind] returns the Tetrad tet with its TetradIndex set to ind.";
SetSpacetimeIndex::usage="SetSpacetimeIndex[tet,ind] returns the Tetrad tet with its SpacetimeIndex set to ind.";
SetTetradPossibleIndices::usage="SetTetradPossibleIndices[tet,posInds] returns the Tetrad tet with its PossibleIndices set to posInds.";


TensorExpression::usage="TensorExpression is a Head created by the command ToTensorExpression.";
ToTensorExpression::usage="ToTensorExpression[expr] returns a TensorExpression that can have its indices manipulated \
as if it were a single Tensor.";
TensorTerms::usage="TensorTerms[te] returns a list of terms that make up the TensorExpression te.";
AllIndices::usage="AllIndices[te] returns a list of all the indices in the TensorExpression te, nested to \
indicate which tensors they belong to.";
AllMetrics::usage="AllMetrics[te] returns a list of metrics that correspond to the different Tensors \
that make up the TensorExpression te.";


TensorTerm;


Begin["`Private`"];


Options[SetTensorKeyValue]={"IgnoreWarnings"->False};
Options[SetMetricQ]=Options[SetTensorKeyValue];
Options[SetMetric]=Options[SetTensorKeyValue];
Options[SetCurve]=Options[SetTensorKeyValue];
Options[SetCurveQ]=Options[SetTensorKeyValue];
Options[SetCurveParameter]=Options[SetTensorKeyValue];
Options[SetIndices]=Options[SetTensorKeyValue];
Options[SetPossibleIndices]=Options[SetTensorKeyValue];
Options[SetSpacetimeDimensions]=Options[SetTensorKeyValue];
Options[SetCoordinates]=Options[SetTensorKeyValue];
Options[SetTensorName]=Options[SetTensorKeyValue];
Options[SetTensorDisplayName]=Options[SetTensorKeyValue];
Options[SetAbstractQ]=Options[SetTensorKeyValue];
Options[SetTensorValues]=Options[SetTensorKeyValue];
Options[SetTetrad]=Options[SetTensorKeyValue];

Options[SetTetradKeyValue]=Options[SetTensorKeyValue];
Options[SetTetradName]=Options[SetTensorKeyValue];
Options[SetTetradDisplayName]=Options[SetTensorKeyValue];
Options[SetTetradVectors]=Options[SetTensorKeyValue];
Options[SetTetradIndex]=Options[SetTensorKeyValue];
Options[SetSpacetimeIndex]=Options[SetTensorKeyValue];
Options[SetTetradPossibleIndices]=Options[SetTensorKeyValue];

DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"] = {"IgnoreWarnings"->"If True, the Tensor's key value will be set regardless of whether it violates built-in warnings."};
DocumentationBuilder`OptionDescriptions["SetMetricQ"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetMetric"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCurve"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCurveQ"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCurveParameter"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetIndices"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetPossibleIndices"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetSpacetimeDimensions"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetCoordinates"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTensorName"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTensorDisplayName"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetAbstractQ"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTensorValues"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTetrad"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];

DocumentationBuilder`OptionDescriptions["SetTetradKeyValue"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTetradName"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTetradDisplayName"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTetradVectors"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTetradIndex"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetSpacetimeIndex"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];
DocumentationBuilder`OptionDescriptions["SetTetradPossibleIndices"]=DocumentationBuilder`OptionDescriptions["SetTensorKeyValue"];


$CacheTensorValues=False;


Tensor/:Format[t_Tensor]:=formatTensor[TensorDisplayName@t,Indices@t,CurveParameter@t,If[(Tetrad@t=!=Undefined)&&(Tetrad@t=!=Blank[]),PossibleTetradIndices[Tetrad@t],{}]]


Clear[indicesStrings]
indicesStrings[inds_,posIndsTet_]:=
Module[{dnStr,upStr},
	dnStr=StringJoin[If[MatchQ[#,-_Symbol|Verbatim[-_Symbol]|-_Pattern|Verbatim[-_]|Verbatim[-__]|Verbatim[-___]],
							If[MemberQ[posIndsTet,#/.-x_:>x],"("<>ToString[#/.-x_:>x]<>")",ToString[#/.-x_:>x]],
							StringJoin@Table["  ",StringLength[If[MemberQ[posIndsTet,#],"("<>ToString[#]<>")",ToString[#]]]]
						]&/@inds];
	upStr=StringJoin[If[Not@MatchQ[#,-_Symbol|Verbatim[-_Symbol]|-_Pattern|Verbatim[-_]|Verbatim[-__]|Verbatim[-___]],
							If[MemberQ[posIndsTet,#],"("<>ToString[#]<>")",ToString[#]],
							StringJoin@Table["  ",StringLength[If[MemberQ[posIndsTet,#/.-x_:>x],"("<>ToString[#/.-x_:>x]<>")",ToString[#/.-x_:>x]]]]]&/@inds];

	{upStr,dnStr}
]


Clear[formatTensorBoxes]
formatTensorBoxes[name_,inds_,param_,posIndsTet_]:=
Module[{upStr,dnStr,out1,nameStr},
	nameStr = If[MatchQ[name,_String],name,ToString[name]];
	out1=If[inds==={},
		nameStr,
		{upStr,dnStr}=indicesStrings[inds,posIndsTet];
		SubsuperscriptBox[nameStr,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
	];
	If[param=!=Undefined,
		out1[ToString[param]],
		out1
	]
]


Clear[formatTensor]
formatTensor[name_,inds_,param_,posIndsTet_]:=DisplayForm@formatTensorBoxes[name,inds,param,posIndsTet]


def@Coordinates[t_Tensor]:=(Association@@t)["Coordinates"];
def@Curve[t_Tensor]:=If[(Association@@t)["Curve"]==="Self",t,(Association@@t)["Curve"]];
def@Rank[t_Tensor]:=Module[{inds,co},inds=Indices[t];co=Count[inds,-_Symbol];{Length[inds]-co,co}];
reDef@Rank[expr_]:=Module[{inds,co},inds=IndicesTraced[expr];co=Count[inds,-_Symbol];{Length[inds]-co,co}];
def@AbstractQ[t_Tensor]:=(Association@@t)["AbstractQ"];
def@SpacetimeDimensions[t_Tensor]:=(Association@@t)["Dimensions"];
def@Indices[t_Tensor]:=(Association@@t)["Indices"];
def@PossibleIndices[t_Tensor]:=(Association@@t)["PossibleIndices"];
def@SubmanifoldsQ[t_Tensor]:=(Association@@t)["SubmanifoldsQ"];
def@MultipleMetricsQ[t_Tensor]:=(Association@@t)["MultipleMetricsQ"];
def@CurveParameter[t_Tensor]:=(Association@@t)["CurveParameter"];
def@CurveQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["CurveQ"];
def@OnCurveQ[t_]:=MatchQ[t,_Tensor]&&(CurveParameter[t]=!=Undefined);
def@TensorName[t_Tensor]:=(Association@@t)["Name"];
reDef@TensorName[Undefined]:=Undefined;
def@TensorDisplayName[t_Tensor]:=(Association@@t)["DisplayName"];
def@IndexPositions[t_Tensor]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[t];
def@MetricQ[t_]:=MatchQ[t,_Tensor]&&(Association@@t)["MetricQ"];
def@Tetrad[t_Tensor]:=(Association@@t)["Tetrad"];


(*reDef@PossibleIndices[expr_]:=PossibleIndices[Metric[expr]]*)


(*reDef@Coordinates[expr_]:=Coordinates[Metric[expr]]*)


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


def@
CurveRules[c1_Tensor?CurveQ]:=Thread[Coordinates@c1->RawTensorValues@c1]


def@
ToTensor[assoc_Association]:=
Module[{},
	
	keysTests[assoc];
	If[assoc["MultipleMetricsQ"],
		tensorConsistentValuesMultiMetricTests[assoc],
		tensorConsistentValuesTests[assoc]
	];

	If[assoc["SubmanifoldsQ"],
		submanifoldsTests[assoc]
	];
	
	tensorCurveTests[assoc];
	If[assoc["Tetrad"]=!=Undefined,tensorTetradTests[assoc]];

	If[#=!=Undefined&&Not[AutoNameQ[assoc["Name"]]]&&$CacheTensorValues,RawTensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];
	
	Tensor@@(Normal@assoc)
]


tensorKeys={"AbstractQ","Coordinates","Curve","CurveParameter","CurveQ","Dimensions",
		"DisplayName","Indices","Metric","MetricQ","MultipleMetricsQ","Name","PossibleIndices",
		"SubmanifoldsQ","Tetrad","Values"};


testDef@
keysTests[assoc_Association]:=
Module[{notAbstrKeys,listKeys,booleanKeys},
	notAbstrKeys={"Metric","Coordinates","Values","PossibleIndices","Dimensions"};
	listKeys={"Coordinates","PossibleIndices","Indices"};
	booleanKeys={"AbstractQ","CurveQ","MetricQ","MultipleMetricsQ","SubmanifoldsQ"};
	
	If[Sort@Keys[assoc]=!=Sort[tensorKeys],
		Print["The following keys are missing in the tensor formation: "<>ToString[Complement[tensorKeys,Keys[assoc]]]];
		Print["The following extra keys were in the tensor formation: "<>ToString[Complement[Keys[assoc],tensorKeys]]];
		AbortVerbose[]
	];
	
	If[Keys[assoc]=!=Sort[tensorKeys],
		Print["Tensors must be formed from keys in alphabetical order."];
		AbortVerbose[]
	];

	If[Not[MatchQ[assoc/@listKeys,{{___}..}]],
		Print["The following values were not given as lists: "<>ToString[If[Head[assoc[#]]=!=List,#,Nothing]&/@listKeys]];
		AbortVerbose[]
	];

	If[Not[assoc["AbstractQ"]]&&MemberQ[assoc[#]&/@notAbstrKeys,Undefined],
		Print["\"AbstractQ\"->False is inconsistent with Undefined values for "<>ToString[If[assoc[#]===Undefined,#,Nothing]&/@notAbstrKeys]];
		AbortVerbose[]
	];
	
	If[Not@MatchQ[assoc/@booleanKeys,{_?BooleanQ..}],
		Print["The following values were not given as booleans: "<>ToString[If[Not[BooleanQ[assoc@#]],#,Nothing]&/@booleanKeys]];
		AbortVerbose[]
	];
]


testDef@
tensorConsistentValuesMultiMetricTests[assoc_Association]:=
Module[{keys,fullPosInds,dims,indsUp,lens,tfList},

	keys={"Indices","Dimensions","Coordinates","Metric","PossibleIndices","Tetrad"};

	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorConsistentValuesMultiMetricTests: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	lens = Length/@{assoc["Coordinates"],assoc["Dimensions"],assoc["Indices"],assoc["Metric"],assoc["PossibleIndices"]};
	If[Not[SameQ@@lens],
		Print["Inconsistent data. When forming a MultipleMetric Tensor, the number of lists of Coordinates, \
SpacetimeDimensions, Indices, PossibleIndices, and Metrics must be the same."];
		Print["Number of lists of ", #, " = ", Length[assoc[#]]]&/@{"Coordinates","Dimensions","Indices","Metric","PossibleIndices"};
		AbortVerbose[];
	];
	
	checkForUniquePossibleIndices[assoc["Metric"],False];

	If[Not@MatchQ[assoc["Coordinates"],{{__Symbol|__Integer}..}],
		Print["Coordinates must be a List of Lists of Symbols or Integers."];
		AbortVerbose[]
	];

	If[Not[MatchQ[assoc["Dimensions"],{_Integer?Positive..}]],
		Print["Dimensions must be a List of positive Integers."];
		AbortVerbose[]
	];
	
	If[Length/@assoc["Coordinates"]=!=assoc["Dimensions"],
		Print["The number of coordinates given does not match the number of SpacetimeDimensions."];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["Indices"]/.-sym_Symbol:>sym,{___Symbol}],
		Print["Indices must be a list of Symbols (and negative Symbols)."];
		AbortVerbose[]
	];
		
	If[Not@MatchQ[assoc["PossibleIndices"],{{__Symbol}..}]||Min[Length/@assoc["PossibleIndices"]]<8,
		Print["PossibleIndices must be a list of at least 8 Symbols"];
		AbortVerbose[]
	];

	If[assoc["Tetrad"]=!=Undefined,
		Print["Tetrads are incompatible with Tensors with MultipleMetrics at this time."];
		AbortVerbose[]
	];
	
	indsUp = assoc["Indices"]/.(-a_Symbol:>a);
	tfList=MapThread[MemberQ[#1,#2]&,{assoc["PossibleIndices"],indsUp}];
	If[DeleteDuplicates@tfList=!={True},
		MapThread[If[Not@#1,Print["Given index ", #2, " not found in List of PossibleIndices ", #3],Nothing]&,{tfList,indsUp,assoc["PossibleIndices"]}];
		AbortVerbose[];
	];

	If[Intersection[Flatten[assoc["Coordinates"]],Flatten[assoc["PossibleIndices"]]]=!={},
		Print["The following elements appear as both indices and coordinates: "<>ToString[Intersection[assoc["Coordinates"],assoc["PossibleIndices"]]]];
		AbortVerbose[]
	];
	
	dims[expr_]:=If[MatchQ[expr,_List],Dimensions[expr],{}];
	If[dims[assoc["Values"]]=!=assoc["Dimensions"],
		Print["Provided values are inconsistent with given MultipleMetric Tensor Rank and/or Dimensions."];
		Print["Expected values of dimensions ", assoc["Dimensions"], " but got ", dims[assoc["Values"]]];
		AbortVerbose[]
	]
]


testDef@
tensorConsistentValuesTests[assoc_Association]:=
Module[{keys,fullPosInds,dims},
	keys={"Indices","Dimensions","Coordinates","PossibleIndices","Tetrad","Values"};

	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorIndicesCoordsTests: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["Coordinates"],{__Symbol|__Integer}],
		Print["Coordinates must be a List of Symbols or Integers."];
		AbortVerbose[]
	];

	If[Not[MatchQ[assoc["Dimensions"],_Integer?Positive]],
		Print["Dimensions must be a positive Integer."];
		AbortVerbose[]
	];
	
	If[Length@assoc["Coordinates"]=!=assoc["Dimensions"],
		Print["The number of coordinates given does not match the number of SpacetimeDimensions."];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["Indices"]/.-sym_Symbol:>sym,{___Symbol}],
		Print["Indices must be a list of Symbols (and negative Symbols)."];
		AbortVerbose[]
	];
		
	If[Not@MatchQ[assoc["PossibleIndices"],{__Symbol}]||Length[assoc["PossibleIndices"]]<8,
		Print["PossibleIndices must be a list of at least 8 Symbols"];
		AbortVerbose[]
	];

	fullPosInds = Join[assoc["PossibleIndices"],If[assoc["Tetrad"]===Undefined,{},PossibleTetradIndices[assoc["Tetrad"]]]];
	If[assoc["Indices"]=!={}&&Not@ContainsAll[fullPosInds,assoc["Indices"]/.(-a_Symbol:>a)],
		Print["Given Indices ", assoc["Indices"]/.(-a_Symbol:>a), " are not all found in List of PossibleIndices ", fullPosInds];
		AbortVerbose[];
	];

	If[Intersection[assoc["Coordinates"],assoc["PossibleIndices"]]=!={},
		Print["The following elements appear as both indices and coordinates: "<>ToString[Intersection[assoc["Coordinates"],assoc["PossibleIndices"]]]];
		AbortVerbose[]
	];
	
	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorConsistentValuesTest: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	dims[expr_]:=If[MatchQ[expr,_List],Dimensions[expr],{}];
	If[dims[assoc["Values"]]=!=Table[assoc["Dimensions"],{Length[assoc["Indices"]]}],
		Print["Provided values are inconsistent with given Tensor Rank and/or Dimensions."];
		AbortVerbose[]
	]

]


(*testDef@
submetricsTests[coords_List,{q11_,q12_,q22_}]:=
Module[{dims,span11,span12,span22,coords11,coords12,coords22},
	
	dims=Length@coords;

	{span11,coords11}
			=If[MatchQ[q11,(_TensorExpression|_Tensor)],
				If[Total@Rank[q11]===2,
					{{SpacetimeDimensions[q11],SpacetimeDimensions[q11]},Coordinates[q11]},
					Print["Cannot define submetrics with a tensor of rank ", Total@Rank[q11]];
					AbortVerbose[]],
				{{1,1},Undefined}];

	{span22,coords22}
			=If[MatchQ[q22,(_TensorExpression|_Tensor)],
				If[Total@Rank[q22]===2,
					{{SpacetimeDimensions[q22],SpacetimeDimensions[q22]},Coordinates[q22]},
					Print["Cannot define submetrics with a tensor of rank ", Total@Rank[q22]];
					AbortVerbose[]],
				{{1,1},Undefined}];

	{span12,coords12}
			=If[MatchQ[q12,(_TensorExpression|_Tensor)],
				{{Total@Rank[q12],SpacetimeDimensions[q12]},Coordinates[q12]},
				{{1,1},Undefined}];

	If[span11+span22=!={dims,dims}||Total@span12=!=dims,
		Print["Submetrics given with dimensions ", span11,", ",span12,
				" and ", span22, " which is inconsistent with a full metric for a spacetime of dimensions ", dims];
		AbortVerbose[]
	];
	If[span11===span12===span22==={1,1},
		Print["Cannot create a submanifold where all sectors are 1x1"];
		AbortVerbose[]
	];
	
	If[Not@MemberQ[{coords11,coords12,coords22},Undefined]]
]*)


reDef@
ToTensor[{name_String,dispName_String},metric_Tensor?MetricQ,vals_,indsGiven_:Undefined]:=
Module[{coords,posInds,dims,inds,nInds},

	If[AbstractQ[metric],Print["Tensor with values cannot be defined using \"AbstractQ\" metric."];AbortVerbose[]];

	coords=Coordinates[metric];	
	posInds=PossibleIndices[metric];
	dims=SpacetimeDimensions[metric];
	nInds=If[MatchQ[vals,_List],Length@Dimensions[vals],0];
	inds=If[indsGiven===Undefined,Take[posInds,nInds],indsGiven];
						
	ToTensor[KeySort@Association["AbstractQ"->False,
						"Coordinates"->coords,
						"Curve"->Undefined,
						"CurveParameter"->Undefined,
						"Dimensions"->dims,
						"DisplayName"->dispName,
						"Indices"->inds,
						"CurveQ"->False,
						"MetricQ"->False,
						"Metric"->metric,
						"MultipleMetricsQ"->False,
						"Name"->name,
						"PossibleIndices"->posInds,
						"SubmanifoldsQ"->SubmanifoldsQ[metric],
						"Tetrad"->Undefined,
						"Values"->vals]]
];
reDef@ToTensor[name_String,metric_Tensor?MetricQ,vals_,indsGiven_:Undefined]:=ToTensor[{name,name},metric,vals,indsGiven];


reDef@
ToTensor[{name_String,dispName_String},{mets__Tensor?MetricQ},vals_,indsGiven_:Undefined]:=
Module[{coords,posInds,dims,inds,nInds},

	If[DeleteDuplicates[AbstractQ/@{mets}]=!={False},
		Print["Tensor with values cannot be defined using \"AbstractQ\" metrics."];AbortVerbose[]];

	coords=Coordinates/@{mets};	
	posInds=PossibleIndices/@{mets};
	dims=SpacetimeDimensions/@{mets};
	nInds=If[MatchQ[vals,_List],Length@Dimensions[vals],0];
	inds=If[indsGiven===Undefined,multiMetricTensorIndices[{mets}],indsGiven];
	
	ToTensor[KeySort@Association["AbstractQ"->False,
						"Coordinates"->coords,
						"Curve"->Undefined,
						"CurveParameter"->Undefined,
						"Dimensions"->dims,
						"DisplayName"->dispName,
						"Indices"->inds,
						"CurveQ"->False,
						"MetricQ"->False,
						"Metric"->{mets},
						"MultipleMetricsQ"->True,
						"Name"->name,
						"PossibleIndices"->posInds,
						"SubmanifoldsQ"->False,
						"Tetrad"->Undefined,
						"Values"->vals]]
];
reDef@ToTensor[name_String,{mets__Tensor?MetricQ},vals_,indsGiven_:Undefined]:=ToTensor[{name,name},{mets},vals,indsGiven];


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


(*def@
Submetrics[t_Tensor?SubmanifoldsQ]:=Diagonal@RawTensorValues[Metric@t]*)


testDef@
builtInIndices[label_String]:=
Switch[label,
		"Latin",
		Symbol/@CharacterRange["a","z"],
		"CapitalLatin",
		Symbol/@Complement[CharacterRange["A","Z"],{"D","C","E","I","K","N","O"}],
		"Greek",
		Symbol/@Complement[CharacterRange["\[Alpha]","\[Omega]"],{"\[Pi]","\[Tau]","\[Chi]"}],
		___,
		Print["No built-in indices ", label, ". Options are \"Latin\", \"CapitalLatin\", and \"Greek\""]; AbortVerbose[]
]


def@
ToMetric[{name_String,dispName_String},coords_List,vals_List,posIndsParam_]:=
Module[{inds,posInds,posIndsFull,dims},

	posIndsFull = 
		If[MemberQ[{"Greek","Latin","CapitalLatin"},posIndsParam],
			builtInIndices[posIndsParam],
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
			Print["To be consistent with the number of given Coordinates, Metric Values must be a ", 
				dims, " \[Times] ", dims, " matrix given as a nested List."];
			AbortVerbose[]
	];
					
	ToTensor[Association[
					"AbstractQ"->False,
					"Coordinates"->coords,
					"Curve"->Undefined,
					"CurveParameter"->Undefined,
					"CurveQ"->False,
					"Dimensions"->dims,
					"DisplayName"->dispName,
					"Indices"->inds,
					"Metric"->"Self",
					"MetricQ"->True,
					"MultipleMetricsQ"->False,
					"Name"->name,
					"PossibleIndices"->posInds,
					"SubmanifoldsQ"->False,
					"Tetrad"->Undefined,
					"Values"->vals]]
];
reDef@ToMetric[{name_String,dispName_String},coords_,vals_]:=ToMetric[{name,dispName},coords,vals,"Greek"]
reDef@ToMetric[name_String,coords_,vals_,posIndsParam_]:=ToMetric[{name,name},coords,vals,posIndsParam]
reDef@ToMetric[name_String,coords_,vals_]:=ToMetric[{name,name},coords,vals,"Greek"]


def@
ToCurve[{name_String,dispName_String},metric_Tensor?MetricQ,vals_List,param_Symbol]:=
Module[{posInds,coords,dims},

	posInds=PossibleIndices[metric];
	coords=Coordinates[metric];
	dims=SpacetimeDimensions[metric];	
	If[dims=!=Length@vals,Print["Number of given values do not match Metric's SpacetimeDimensions"]; AbortVerbose[]];
	
	checkForParam[vals,coords,param];

	ToTensor[KeySort@Association["AbstractQ"->False,
								"Coordinates"->coords,
								"Curve"->"Self",
								"CurveParameter"->param,
								"Dimensions"->dims,
								"DisplayName"->dispName,
								"Indices"->{First@posInds},
								"CurveQ"->True,
								"MetricQ"->False,
								"Metric"->metric,
								"MultipleMetricsQ"->False,
								"Name"->name,
								"PossibleIndices"->posInds,
								"SubmanifoldsQ"->SubmanifoldsQ[metric],
								"Tetrad"->Undefined,
								"Values"->vals]]
]
reDef@
ToCurve[name_String,metric_Tensor?MetricQ,vals_List,param_Symbol]:=ToCurve[{name,name},metric,vals,param];


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
Metric[t_Tensor]:=Which[(Association@@t)["Metric"]==="Self",
								t,
								Curve@t=!=Undefined,
								ToTensorFieldOnCurve[(Association@@t)["Metric"],Curve@t],
								True,
								(Association@@t)["Metric"]
							];


reDef@
Metric[expr_]:=
Module[{metrics,metricNames},

	metrics=Cases[{expr},m_Tensor/;MetricQ[m],Infinity];
	If[metrics==={},
		(*Print["Expression ",expr, " does not contain a metric."]; AbortVerbose[]*)
		Undefined,
		metricNames=DeleteDuplicates[TensorName/@metrics];
		If[Not[SameQ@@metricNames],
			(*Print["Expression contains Tensors with different metrics: ",metricNames ]; AbortVerbose[]*)
			Indeterminate,
			First@metrics
		]
	]
]


def@
InverseMetric[t_Tensor?MetricQ]:=InverseMetric[t]=
Module[{assoc,tvStored,tv,posUp},

	posUp={"Up","Up"};
	tvStored=RawTensorValues[TensorName[t],posUp];
	tv=If[tvStored===Undefined,
			If[TensorValues[t]===Undefined,
				Undefined,
				If[SubmanifoldsQ[t],
					DiagonalMatrix[InverseMetric/@Submetrics[t]],
					Simplify[Inverse@RawTensorValues[Metric[t]]]
				]
			],
			tvStored
		];
	
	assoc=Association@@t;
	ToTensor[KeySort@Join[assoc,
					Association["Indices"->Indices[t]/.-sym_Symbol:>sym,
								"Values"->tv,
								"Metric"->Metric[t]]]]
];
reDef@
InverseMetric[t_Tensor]:=If[Metric@t===Undefined,Undefined,InverseMetric@Metric@t];


reDef@InverseMetric[t_Tensor?MultipleMetricsQ]:=If[Metric@t===Undefined,Undefined,InverseMetric/@Metric[t]];


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
SetCurve[t_Tensor,c_Tensor,opts:OptionsPattern[]]:=If[t=!=c,SetTensorKeyValue[t,"Curve",c],t,opts]


def@
SetCurveQ[t_Tensor,bool_?BooleanQ,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"CurveQ",bool,opts]


def@
SetCurveParameter[t_Tensor,param_Symbol,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"CurveParameter",param,opts]


def@
SetIndices[t_Tensor,inds___List,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Indices",inds,opts]


def@
SetPossibleIndices[t_Tensor,inds_List,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"PossibleIndices",inds,opts]


def@
SetSpacetimeDimensions[t_Tensor,dims_Integer?NonNegative,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Dimensions",dims,opts]


def@
SetCoordinates[t_Tensor,coords_List,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Coordinates",coords,opts]


def@
SetTensorName[t_Tensor,{name_String,dispName_String},opts:OptionsPattern[]]:=SetTensorDisplayName[SetTensorKeyValue[t,"Name",name,opts],dispName,opts]
reDef@
SetTensorName[t_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Name",name,opts]


def@
SetTensorDisplayName[t_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"DisplayName",name,opts]


def@
SetAbstractQ[t_Tensor,bool_?BooleanQ,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"AbstractQ",bool,opts]


def@
SetTensorValues[t_Tensor,values_List,opts:OptionsPattern[]]:=(ClearCachedTensorValues[t];SetTensorKeyValue[t,"Values",values,opts])
reDef@
SetTensorValues[t_Tensor/;Rank[t]==={0,0},values_,opts:OptionsPattern[]]:=(ClearCachedTensorValues[t];SetTensorKeyValue[t,"Values",values,opts])


def@
SetTetrad[t_Tensor,tet_Tetrad,opts:OptionsPattern[]]:=SetTensorKeyValue[t,"Tetrad",tet,opts]


def@
ToTensorFieldOnCurve[t1_Tensor,c1_?CurveQ]:=
Module[{params},
	
	If[TensorName@Metric@t1=!=TensorName[(Association@@c1)["Metric"]],
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
	params = {t1,{"Name",TensorName[t1]<>"Curve"},{"Values",vals},{"Curve",c1},{"CurveParameter",CurveParameter@c1},{"MetricQ",False},{"Metric",Metric[t1]}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]


reDef@
ToTensorOnCurve[{name_String,displayName_String},c1_?CurveQ,vals_List,inds_:Undefined]:=
Module[{t1,params},
	t1=ToTensor[{name,displayName},Metric[c1],vals,inds];
	params = {t1,{"Curve",c1},{"CurveParameter",CurveParameter@c1}};
	Fold[SetTensorKeyValue[#1,Sequence@@#2]&,params]
]
reDef@
ToTensorOnCurve[name_String,c1_?CurveQ,vals_List,inds_:Undefined]:=ToTensorOnCurve[{name,name},c1,vals,inds]


reDef@
Indices[expr_]:=
Module[{terms,inds},
	ValidateTensorExpression[expr];
	terms=toListOfTerms[expr];
	inds=(indicesAndMetricsInTerm[#,False]&/@terms)[[All,All,1]];
	If[Not[SameQ@@inds],Print["The expression does not have unique indices. Call IndicesTraced to get a unique list."];AbortVerbose[]];
	First@inds
]


reDef@
IndicesTraced[expr_]:=
Module[{terms,inds},
	ValidateTensorExpression[expr];
	terms=toListOfTerms[expr];
	inds=(indicesAndMetricsInTerm[#,False]&/@terms)[[All,All,1]];
	UniqueIndices@First@inds
]


def@
ValidTensorExpressionQ[expr_]:=ValidateTensorExpression[expr,True]


Clear[toListOfTerms]
toListOfTerms[expr_]:=Which[MatchQ[expr,_Plus],List@@expr,True,{expr}]


testDef@
indicesAndMetricsInTerm[term_,test_?BooleanQ]:=
Module[{termExpand,tensorMetricIndices},
	termExpand=Expand[term];
	tensorMetricIndices[t1_Tensor?MultipleMetricsQ]:=Thread[{Indices[t1],Metric[t1]}];
	tensorMetricIndices[t2_Tensor]:={#,Metric[t2]}&/@Indices[t2];
	
	Which[
		MatchQ[termExpand,Power[_Tensor,_]|__ Power[_Tensor,_]],If[test,False,Print["Tensors cannot be raised to a power as in ", termExpand]; AbortVerbose[]],
		MatchQ[termExpand,_Times],Join@@Sort[Cases[termExpand,t_Tensor:>tensorMetricIndices[t],{1}]],
		MatchQ[termExpand,_Tensor],tensorMetricIndices[termExpand],
		True,If[test,False,Print["Expression should be a Tensor or a Tensor product, but is ", termExpand]; AbortVerbose[]]
	]
];


Clear[uniqueMetrics]
uniqueMetrics[indMetTerm_]:=
Module[{uniqueInds,indsMets},
	uniqueInds=UniqueIndices[indMetTerm[[All,1]]];
	indsMets=Table[Flatten@Select[indMetTerm,#[[1]]===ind&],{ind,uniqueInds}];
	indsMets[[All,2]]
]


Clear[RepeatedIndices]
RepeatedIndices[inds_]:=
Module[{toCov,indsUp},
	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	indsUp=toCov[inds];
	Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp])
]


Clear[UniqueIndices]
UniqueIndices[inds_]:=Sort@DeleteCases[inds,Alternatives@@Flatten@RepeatedIndices[inds]]


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


testDef@
checkForUniquePossibleIndices[mets_,test_?BooleanQ]:=
Module[{metsPosInds,allInds},
	metsPosInds=DeleteDuplicates[{TensorName[#],PossibleIndices[#]}&/@mets];
	If[Length@metsPosInds>1,
		allInds=Flatten[metsPosInds[[All,2]]];
		If[Not@DuplicateFreeQ[allInds],
			If[test,Return@False,
				Print["The following Indices are given in lists of PossibleIndices for more than one metric: ", DeleteDuplicates[Select[allInds,Count[allInds,#]>1&]]];
				AbortVerbose[]
			]
		]
	];
	If[test,True]
]


def@
ValidateTensorExpression[expr_,test_?BooleanQ]:=
Module[{terms,tfList1,tfList2,indicesMetricsList,sumQ,mets,metNames,exprExpand,inds},

	exprExpand=Expand[expr];
	terms=toListOfTerms[expr];
	
	indicesMetricsList=indicesAndMetricsInTerm[#,test]&/@terms;
	If[test&&Cases[indicesMetricsList,False]=!={},Return@False];
	inds=indicesMetricsList[[All,All,1]];

	tfList1=checkForUniquePossibleIndices[#[[All,2]],test]&/@indicesMetricsList;
	If[test&&DeleteDuplicates[tfList1]=!={True},Return@False];

	tfList2=ValidateIndices[#,test]&/@inds;
	If[test&&DeleteDuplicates[tfList2]=!={True},Return@False];

	sumQ=SameQ@@(Sort/@UniqueIndices/@inds);
	If[Not@sumQ,If[test,Return@False,Print["Cannot add Tensors with different indices as in ", expr];AbortVerbose[]]];
	
	mets=uniqueMetrics/@indicesMetricsList;
	metNames=Map[TensorName,mets,{2}];
	If[Not[SameQ@@metNames],If[test,Return@False,Print["Cannot add Tensors with different metrics: ", mets ]; AbortVerbose[]]];

	If[test,True]
]
reDef@ValidateTensorExpression[expr_]:=ValidateTensorExpression[expr,False];


Tetrad/:Format[t_Tetrad]:=formatTetrad[TetradDisplayName@t,Indices@t]
Clear[formatTetrad]
formatTetrad[name_String,{tetInd_,stInd_}]:=
Module[{upStr,dnStr,inds},

	inds = {tetInd,stInd};

		dnStr=StringJoin[If[MatchQ[#,-_Symbol],ToString[#/.-x_:>x],StringJoin@Table["  ",StringLength[ToString[#/.-x_:>x]]]]&/@inds];
		upStr=StringJoin[If[Not@MatchQ[#,-_Symbol],ToString[#],StringJoin@Table["  ",StringLength[ToString[#/.-x_:>x]]]]&/@inds];
		DisplayForm@SubsuperscriptBox[name,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
]


def@
ToTetrad[assoc_Association]:=
Module[{keys,listKeys,index},
	keys={"Name","DisplayName","Index","PossibleIndices","Vectors","Metric"};
	listKeys={"PossibleIndices"};

	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the tetrad formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the tetrad formation: "<>ToString[Complement[Keys[assoc],keys]]];
		AbortVerbose[]
	];
	
	If[Keys[assoc]=!=Sort[keys],
		Print["Tetrad must be formed from keys in alphabetical order."];
		AbortVerbose[]
	];

	If[Not[MatchQ[assoc["Name"],_String]],
		Print["Tetrad Name must be a String."];
		AbortVerbose[]
	];

	If[Not[MatchQ[assoc["DisplayName"],_String]],
		Print["Tetrad DisplayName must be a String."];
		AbortVerbose[]
	];
	
	If[Not[MatchQ[assoc["Vectors"],{_Tensor,_Tensor,_Tensor,_Tensor}]]||(DeleteDuplicates[Rank/@assoc["Vectors"]]=!={{1,0}}&&DeleteDuplicates[Rank/@assoc["Vectors"]]=!={{0,1}}),
		Print["Tetrad must be formed from four contravariant vectors or four covariant vectors."];
		AbortVerbose[]
	];

	If[Length@DeleteDuplicates[TensorName/@assoc["Vectors"]]=!=4,
		Print["Cannot form tetrad without four unique vectors."];
		AbortVerbose[]
	];

	If[Length@DeleteDuplicates[TensorName[Metric[#]]&/@assoc["Vectors"]]=!=1,
		Print["Cannot form tetrad from vectors with inconsistent metrics."];
		AbortVerbose[]
	];

	If[Length@DeleteDuplicates[Coordinates/@assoc["Vectors"]]=!=1,
		Print["Cannot form tetrad from vectors with inconsistent Coordinates."];
		AbortVerbose[]
	];

	If[Length@DeleteDuplicates[PossibleIndices/@assoc["Vectors"]]=!=1,
		Print["Cannot form tetrad from vectors with inconsistent PossibleIndices Lists."];
		AbortVerbose[]
	];

	If[Length@DeleteDuplicates[Indices/@assoc["Vectors"]]=!=1,
		Print["Cannot form tetrad from vectors with inconsistent Indices."];
		AbortVerbose[]
	];

	If[DeleteDuplicates[SpacetimeDimensions/@assoc["Vectors"]]=!={4},
		Print["Tetrads are currently only supported in 4 SpacetimeDimensions."];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["PossibleIndices"],{__Symbol}]||Length@assoc["PossibleIndices"]<8,
		Print["PossibleIndices must be a list of at least 8 Symbols"];
		AbortVerbose[]
	];

	If[Intersection[PossibleIndices[assoc["Vectors"][[1]]],assoc["PossibleIndices"]]=!={},
		Print["Tetrad indices cannot be the same as spacetime indices of the tetrad vectors."];
		AbortVerbose[]
	];

	If[assoc["Index"]=!=Undefined&&Not@MemberQ[assoc["PossibleIndices"],assoc["Index"]/.(-a_Symbol:>a)],
		Print["Given Index ", assoc["Index"]/.(-a_Symbol:>a), " not found in List of with PossibleIndices ", assoc["PossibleIndices"]];
		AbortVerbose[];
	];

	index=If[assoc["Index"]===Undefined,-assoc["PossibleIndices"][[1]],assoc["Index"]];

(*	If[#=!=Undefined&&Not[AutoNameQ[assoc["Name"]]]&&$CacheTensorValues,RawTensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];*)
	Tetrad@@(Normal@assoc/.("Index"->_):>("Index"->index))
]


reDef@
ToTetrad[{n_String,dn_String},vecs_List,posInds_]:=
Module[{assocMet,metric,posIndsComp,posIndsSyms},

	posIndsSyms = If[MemberQ[{"Greek","Latin","CapitalLatin"},posInds],builtInIndices[posInds],posInds];
	If[Not@MatchQ[posIndsSyms,{__Symbol}],
		Print["PossibleIndices must be a list of Symbols"];
		AbortVerbose[]
	];
	
	posIndsComp=Complement[posIndsSyms,Coordinates[First@vecs]];

	metric = defTetradMetric[n,vecs,posIndsComp];

	ToTetrad[KeySort@Association["DisplayName"->dn,"Index"->-First[posIndsComp],"Metric"->metric,"Name"->n,"PossibleIndices"->posIndsComp,"Vectors"->vecs]]
];
reDef@
ToTetrad[n_String,vecs_List,posInds_List]:=ToTetrad[{n,n},vecs,posInds]


Clear[defTetradMetric]
defTetradMetric[name_,vecs_,posIndsTet_]:=
Module[{tvsTet,tvsMet,tvsTetMet},
	
	tvsTet=Simplify[TensorValues[#]]&/@vecs;
	tvsMet=TensorValues[Metric@First@vecs];
	tvsTetMet = Simplify[tvsTet.tvsMet.Transpose[tvsTet]];

	ToMetric[{name<>"TetradMetric","\[DoubleStruckG]"},{1,2,3,4},tvsTetMet,posIndsTet]
]


def@TetradVectors[t_Tetrad]:=(Association@@t)["Vectors"];
reDef@Coordinates[t_Tetrad]:=Coordinates@First[TetradVectors[t]];
def@SpacetimeIndex[t_Tetrad]:=Indices[First[TetradVectors[t]]][[1]];
def@TetradIndex[t_Tetrad]:=(Association@@t)["Index"];
reDef@Indices[t_Tetrad]:={TetradIndex[t],SpacetimeIndex[t]};
reDef@SpacetimeDimensions[t_Tetrad]:=SpacetimeDimensions@First[TetradVectors[t]];
def@PossibleSpacetimeIndices[t_Tetrad]:=PossibleIndices@First[TetradVectors[t]];
def@PossibleTetradIndices[t_Tetrad]:=(Association@@t)["PossibleIndices"];
def@TetradName[t_Tetrad]:=(Association@@t)["Name"];
def@TetradDisplayName[t_Tetrad]:=(Association@@t)["DisplayName"];
reDef@IndexPositions[t_Tetrad]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[t];
def@SpacetimeMetric[t_Tetrad]:=Metric@First[TetradVectors[t]];
def@TetradMetric[t_Tetrad]:=(Association@@t)["Metric"];


def@
SetTetradKeyValue[t_Tetrad,key_String,value_,opts:OptionsPattern[]]:=
Module[{tests},
	tests = {"IgnoreWarnings" ->{BooleanQ,"IgnoreWarnings of MergeNested must be True or False."}};
	TestOptions[tests,{opts}];
	
	If[OptionValue["IgnoreWarnings"],
		Tetrad[Normal[KeySort@Join[KeyDrop[(Association@@t),{key}],Association[key->value]]]],
		ToTetrad[KeySort@Join[KeyDrop[(Association@@t),{key}],Association[key->value]]]
	]
]


def@
SetTetradName[t_Tetrad,{name_String,dispName_String},opts:OptionsPattern[]]:=SetTetradDisplayName[SetTetradKeyValue[t,"Name",name,opts],dispName,opts]
reDef@
SetTetradName[t_Tetrad,name_String,opts:OptionsPattern[]]:=SetTetradKeyValue[t,"Name",name,opts]


def@
SetTetradDisplayName[t_Tetrad,name_String,opts:OptionsPattern[]]:=SetTetradKeyValue[t,"DisplayName",name,opts]


def@
SetTetradVectors[t_Tetrad,vecs_List,opts:OptionsPattern[]]:=SetTetradKeyValue[t,"Vectors",vecs,opts]


def@
SetTetradIndex[t_Tetrad,ind_,opts:OptionsPattern[]]:=SetTetradKeyValue[t,"Index",ind,opts]


def@
SetSpacetimeIndex[t_Tetrad,ind_,opts:OptionsPattern[]]:=SetTetradVectors[t,Through[TetradVectors[t][ind]],opts]


def@
SetTetradPossibleIndices[t_Tetrad,posInds_List,opts:OptionsPattern[]]:=SetTetradKeyValue[t,"PossibleIndices",posInds,opts]


def@
TetradValues[t_Tetrad]:=
Module[{indsPos,vecsUpDn,metId},

	indsPos = IndexPositions[t];	
	
	metId=If[indsPos[[1]]==="Down",
			IdentityMatrix[4],
			TensorValues[InverseMetric[TetradMetric[t]]]
		];

	vecsUpDn=TensorValues/@TetradVectors[t];

	metId.vecsUpDn
]


def@
ToTensorWithTetrad[t_Tensor,tet_Tetrad]:=
Module[{},
	SetTetrad[t,tet]
]


def@
SingleMetricQ[t_Tensor]:=MatchQ[Metric[t],_Tensor?MetricQ|"Self"]


testDef@
tensorSubmetricTests[assoc_Association]:=
Module[{coords,subsets,keys},

	keys={"Values","Coordinates","PossibleIndices"};	
	
	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorSubmetricTests: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	If[DiagonalMatrix@Diagonal[assoc["Values"]]=!=assoc["Values"],
		Print["Submetrics can only be defined along the diagonal."];
		AbortVerbose[]
	];

(*	If[Length@assoc["Values"]===1,
		Print["It doesn't really make sense to have only one submetric, does it?"];
		AbortVerbose[]
	];*)

	coords=Flatten[Coordinates/@assoc["Values"]];
	If[Sort@coords === Sort[assoc["Coordinates"]],
		If[coords =!= assoc["Coordinates"],
			Print["Submetrics must be given so their Coordinates are in the same order as the Metric."];
			Print["Metric Coordinates are ", assoc["Coordinates"]];
			Print["Submanifold Coordinates are ", coords];
			AbortVerbose[]
		],
			
		Print["Together, the Submetrics must have the same Coordinates as the Metric."];
		AbortVerbose[]
	];

	subsets=Subsets[Join[{assoc["PossibleIndices"]},PossibleIndices/@assoc["Values"]],{2}];
	If[Flatten[Intersection@@#&/@subsets]=!={},
		Print["The Metric and each of its Submetrics must have a unique List of PossibleIndices."];
		Print["The following Symbols are used in multiple metrics: ",Flatten[Intersection@@#&/@subsets]];	
		AbortVerbose[]
	];	
]


testDef@
tensorMetricTests[assoc_Association]:=
Module[{keys},
	keys={"Metric","Coordinates","Dimensions","PossibleIndices"};

	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorMetricTests: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	If[assoc["Metric"]===Undefined,
		Print["All Tensors must have an associated Metric."];
		AbortVerbose[]
	];

	If[Not@MatchQ[assoc["Metric"],_List],
	
		If[Not@MetricQ[assoc["Metric"]],
			Print["Given Option \"Metric\" is neither a Metric Tensor, nor a List of Metric Tensors."];
			AbortVerbose[]
		];
		
		If[(assoc["Coordinates"] =!= Coordinates[assoc["Metric"]]),
			Print["Metric's Coordinates and given Coordinates are inconsistent: ", Coordinates[assoc["Metric"]], " and ",assoc["Coordinates"]];
			AbortVerbose[]
		];

		If[(assoc["Dimensions"] =!= SpacetimeDimensions[assoc["Metric"]]),
			Print["Metric's SpacetimeDimensions and given SpacetimeDimensions are inconsistent: ", SpacetimeDimensions[assoc["Metric"]], " and ",assoc["Dimensions"]];
			AbortVerbose[]
		];
	
		If[Sort[assoc["PossibleIndices"]]=!= Sort[PossibleIndices[assoc["Metric"]]],
			Print["Metric's PossibleIndices and given PossibleIndices are inconsistent: ", PossibleIndices[assoc["Metric"]], " and ",assoc["PossibleIndices"]];
			AbortVerbose[]
		]
	]
]


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
tensorTetradTests[assoc_Association]:=
Module[{keys},
	keys={"Tetrad","Dimensions","Metric","PossibleIndices","Name"};

	If[Complement[keys,Keys[assoc]]=!={},
		Print["The following keys are missing in the tensorTetradTests: "<>ToString[Complement[keys,Keys[assoc]]]];
		AbortVerbose[]
	];

	If[Not[MatchQ[assoc["Tetrad"],_Tetrad]],
		Print["\"Tetrad\" must be Undefined or have Head Tetrad."];
		AbortVerbose[]
	];
		
	If[SpacetimeDimensions[assoc["Tetrad"]]=!=assoc["Dimensions"],
		Print["Tetrad and Tensor must have same SpacetimeDimensions."];
		AbortVerbose[]
	];
	
	If[assoc["Metric"]===Undefined,
		Print["Tetrads can only be assigned to Tensors with metrics."];
		AbortVerbose[]
	];
	
	If[If[assoc["Metric"]==="Self",assoc["Name"],TensorName[assoc["Metric"]]]=!=TensorName[SpacetimeMetric[assoc["Tetrad"]]],
		Print["Tetrad's spacetime Metric and Tensor's Metric are not the same."];
		AbortVerbose[]
	];
	
	If[Sort@PossibleSpacetimeIndices[assoc["Tetrad"]]=!=Sort@assoc["PossibleIndices"],
		Print["Tetrad's PossibleSpacetimeIndices and Tensor's PossibleIndices are not the same."];
		AbortVerbose[]
	];
]


def@
ToTensorExpression[assoc_Association]:=
Module[{te},
	(* Still need to write this ... other tests?*)
	(* keysTestsTE[assoc];*) 
	
	te=TensorExpression@@(Normal@KeySort@assoc);
	ValidateTensorExpression[te];
	te
]


reDef@
ToTensorExpression[expr_,displayName_:Undefined]:=
Module[{tte,l1,l2,indsMets,expandExpr},
	expandExpr=Expand@expr;
	ValidateTensorExpression[expandExpr];
	l1=toListOfTerms[expandExpr];
	l2=Replace[l1,{a___ ts___Tensor:>TensorTerm[Times[a,1],ts],t_Tensor:>TensorTerm[1,t]},1];
	indsMets=indicesAndMetricsInTerm[First@l1,False];
	ToTensorExpression[KeySort@Association["Terms"->l2,
										"Indices"->UniqueIndices[indsMets[[All,1]]],
										"Metric"->uniqueMetrics@indsMets,
										"DisplayName"->displayName]]
]


TensorExpression/:Format[te_TensorExpression]:=
Module[{exprBox,inds,upStr,dnStr,dispName},
	dispName=If[(Association@@te)["DisplayName"]===Undefined,			
					Normal[te]/.{t_Tensor:>formatTensorBoxes[TensorDisplayName@t,Indices@t,CurveParameter@t,If[(Tetrad@t=!=Undefined)&&(Tetrad@t=!=Blank[]),PossibleTetradIndices[Tetrad@t],{}]]},
					(Association@@te)["DisplayName"]];
	exprBox=RowBox[{"[",dispName,"]"}];
	inds=(Association@@te)["Indices"];

	{upStr,dnStr}=indicesStrings[inds,{}];

	(*DisplayForm@SubsuperscriptBox[exprBox,Style[StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],Bold],Style[StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""],FontWeight->Bold]]*)
	DisplayForm@SubsuperscriptBox[exprBox,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
]


TensorExpression/:Normal[te_TensorExpression]:=Total[(Association@@te)["Terms"]/.TensorTerm[ts__]:>Times[ts]]
TensorExpression/:Indices[te_TensorExpression]:=(Association@@te)["Indices"]
TensorExpression/:Metric[te_TensorExpression]:=(Association@@te)["Metric"]
TensorExpression/:PossibleIndices[te_TensorExpression]:=PossibleIndices/@Metric[te]
TensorExpression/:Coordinates[te_TensorExpression]:=Coordinates/@Metric[te]
TensorExpression/:SpacetimeDimensions[te_TensorExpression]:=SpacetimeDimensions/@Metric[te]
TensorExpression/:TensorExpressionDisplayName[te_TensorExpression]:=(Association@@te)["DisplayName"]


reDef@ValidateTensorExpression[te_TensorExpression,test_?BooleanQ]:=ValidateTensorExpression[Normal@te,test]
reDef@ValidateTensorExpression[te_TensorExpression]:=ValidateTensorExpression[te,False]


reDef@ValidTensorExpressionQ[te_TensorExpression]:=ValidateTensorExpression[te,True]


def@TensorTerms[te_TensorExpression]:=(Association@@te)["Terms"]
def@AllIndices[te_TensorExpression]:=TensorTerms[te]/.(TensorTerm[a_,t__Tensor]:>Indices/@{t})
def@AllMetrics[te_TensorExpression]:=TensorTerms[te]/.(TensorTerm[a_,t__Tensor]:>Metric/@{t})


def@SetTensorExpressionKeyValue[te_TensorExpression,key_String,value_]:=ToTensorExpression[KeySort@Join[KeyDrop[(Association@@te),{key}],Association[key->value]]]
def@SetTensorExpressionTerms[te_TensorExpression,terms_]:=SetTensorExpressionKeyValue[te,"Terms",terms];
def@SetTensorExpressionIndices[te_TensorExpression,inds_]:=SetTensorExpressionKeyValue[te,"Indices",inds];
def@SetTensorExpressionMetric[te_TensorExpression,met_]:=SetTensorExpressionKeyValue[te,"Metric",met];
def@SetTensorExpressionDisplayName[te_TensorExpression,dn_]:=SetTensorExpressionKeyValue[te,"DisplayName",dn];


t_Tensor[patternInds__]/;MatchQ[{patternInds},{Repeated[_Pattern|-_Pattern|Verbatim[_]|Verbatim[__]|Verbatim[___]|Verbatim[-_]|Verbatim[-__]|Verbatim[-___]]}]:=TensorPattern[t,{patternInds}]


posIndPatterns={_Pattern,-_Pattern,
				Verbatim[_],Verbatim[-_],
				Verbatim[_Symbol],Verbatim[-_Symbol],
				Verbatim[__],Verbatim[-__],
				Verbatim[__Symbol],Verbatim[-__Symbol],
				Verbatim[___],Verbatim[-___],
				Verbatim[___Symbol],Verbatim[-___Symbol]};


def@
TensorPattern[t_Tensor,patternInds_List]:=
Module[{pis,inds,params,a},

	If[Total@Rank[t]=!=Length@patternInds,
		If[Length@patternInds =!= 1 || (patternInds=!={__} && patternInds=!={___} && Not[MatchQ[patternInds,{Pattern[a,__]}]] && Not[MatchQ[patternInds,{Pattern[a,___]}]]),
			Print["TensorPattern called with ",  Length@patternInds , " Pattern indices, but the Tensor ", t, " is rank ", Rank[t]];
			AbortVerbose[]
		]
	];
	
	Tensor@@Normal[KeySort@Join[KeyDrop[Association@@t,{"Indices","Values","Metric","Curve"}],
					Association["Values"->_,
								"Metric"->_,
								"Curve"->_,
								"Indices"->patternInds]]]
]/;MatchQ[patternInds,{Repeated[Alternatives@@posIndPatterns]}]


reDef@
TensorPattern[p_,patternInds_List]:=
Module[{assoc},
	If[Not@MatchQ[patternInds,{Repeated[Alternatives@@posIndPatterns]}],
		Print["TensorPattern called with invalid index patterns: ",  patternInds];
		AbortVerbose[]
	];
	If[Not@MatchQ[p,Verbatim[_]|Pattern[a,_]|Verbatim[_Tensor]|Pattern[a,Blank[Tensor]]],
		Print["TensorPattern called with invalid Tensor name pattern: ", p];
		AbortVerbose[]
	];
	
	assoc=KeySort[{"AbstractQ"->_,
			"Coordinates"->_,
			"Curve"->_,
			"CurveParameter"->_,
			"Dimensions"->_,
			"DisplayName"->_,
			"Indices"->patternInds,
			"CurveQ"->_,
			"MetricQ"->_,
			"Metric"->_,
			"MultipleMetricsQ"->_,
			"Name"->p,
			"PossibleIndices"->_,
			"SubmanifoldsQ"->_,
			"Tetrad"->_,
			"Values"->_}];
	
	If[Keys@assoc =!= Sort@tensorKeys, 
		Print["Attempting to form TensorPattern with incompatible keys: ", Join[Complement[Sort@tensorKeys, Keys@assoc], Complement[Keys@assoc,Sort@tensorKeys]]];
		AbortVerbose[]];
			
	Tensor@@Normal[assoc]
]


End[];

EndPackage[];
