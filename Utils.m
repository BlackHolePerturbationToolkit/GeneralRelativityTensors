(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`Utils`"];


def;
reDef;
defCond;
reDefCond;
Aborting;

TestOptions;


Begin["`Private`"];


Unprotect[def];
ClearAll[def];
SetAttributes[def,HoldAll];
def/:SetDelayed[def[f_[args___]],rhs_]:=
	(Clear[f];
	f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	f[args]:=Block[{$CurrentFunction=f},rhs])
Protect[def];


Unprotect[reDef];
ClearAll[reDef];
SetAttributes[reDef,HoldAll];
reDef/:SetDelayed[reDef[f_[args___]],rhs_]:=
	(f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	f[args]:=Block[{$CurrentFunction=f},rhs])
Protect[reDef];


(*Unprotect[defCond];
ClearAll[defCond];
SetAttributes[defCond,HoldAll];
defCond/:SetDelayed[Condition[defCond[f_[args___]],test_],rhs_]:=
	(Clear[f];
	f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	Condition[f[args],test]:=Block[{$CurrentFunction=f},rhs])
Protect[defCond];*)


(*Unprotect[reDefCond];
ClearAll[reDefCond];
SetAttributes[reDefCond,HoldAll];
reDefCond/:SetDelayed[Condition[reDefCond[f_[args___]],test_],rhs_]:=
	(f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	SetDelayed[Condition[f[args],test],Block[{$CurrentFunction=f},rhs]])
Protect[reDefCond];*)


Unprotect[testDef];
ClearAll[testDef];
SetAttributes[testDef,HoldAll];
testDef/:SetDelayed[testDef[f_[args___]],rhs_]:=
	(f[x___]:=(Print["Invalid call to "<> ToString[f]<>"[]. Unexpected parameters: ",{x}];Abort[]);
	f[args]:=Block[{},rhs])
Protect[testDef];


testDef@
TestOptions[optsTests_List,optsGiven_List,funcName_:$CurrentFunction]:=
Module[{failList,testList,badTestList,
		optsDefault,optsTestsStr,optsGivenFlatStr,
		optsDefaultStr,defaultUsed,optsUsed},
	
	(* First test the default options *)
	optsDefault = Options[funcName];
	optsDefaultStr = MakeKeysStrings@optsDefault;
	optsTestsStr = MakeKeysStrings@optsTests;

	(* Test that the default options and tests lists are compatible *)
	compLists[optsTestsStr,optsDefaultStr,"Tests for Options","Default Options",funcName];

	testList = testValue[optsTestsStr, #] & /@ optsDefaultStr;
	failList = Flatten@Position[testList, False];
	badTestList = DeleteCases[testList,True|False];

	If[Length@badTestList=!=0,	
		Print["Options test "<> ToString@#<> " failed to evaluate to True or False."]&/@badTestList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];

	If[Length@failList=!=0,
		Print[StringForm["Default Option '`1`' did not satisfy test. `2`", 
				optsDefaultStr[[#]], (optsDefaultStr[[#]]/.optsTestsStr)[[1]]]] & /@ failList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];

	(* Now test the passed options *)
	optsGivenFlatStr = MakeKeysStrings@Flatten@optsGiven;
	defaultUsed=#->(#/.optsDefaultStr)&/@Complement[optsDefaultStr[[All,1]],optsGivenFlatStr[[All,1]]];
	optsUsed=Union[defaultUsed,optsGivenFlatStr];
	
	(* Test that the used options and tests lists are compatible *)
	compLists[optsTestsStr,optsUsed,"Tests for Options","Passed Options",funcName];
	
	testList = testValue[optsTestsStr, #] & /@ optsUsed;
	failList = Flatten@Position[testList, False];
	
	If[Length@failList=!=0,
		Print[StringForm["Passed Option '`1`' did not satisfy test.\n`2`", optsUsed[[#]], (optsUsed[[#]]/.optsTestsStr)[[1,2]]]] & /@ failList;
		Print["Aborting in "<>ToString[funcName]<>"[]."];
		Abort[];
	];
];


testDef@
testValue[optsTests_List,opt_Rule]:=
testValue[optsTests,opt]=
Module[{key,value,test},
	
	value=opt[[2]];
	key=opt[[1]];
	test=(key/.optsTests)[[1]];

	test[value]
]


testDef@
compLists[l1_List,l2_List,str1Name_String,str2Name_String,funcName_:$CurrentFunction]:=
Module[{dups,comp},
	
	If[!(Sort@l2[[All,1]]===Sort@l1[[All,1]]===Union[l1[[All,1]],l2[[All,1]]]),

		dups=(Flatten@Position[l1[[All,1]],#])&/@Union@l1[[All,1]];
		If[Length@#=!=1,
			Print["Multiple values for element " <> ToString@l1[[#]]<>" in "<>str1Name];
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		]&/@dups;
	
		dups=(Flatten@Position[l2[[All,1]],#])&/@Union@l2[[All,1]];
		If[Length@#=!=1,
			Print["Multiple values for element " <> ToString@l2[[#]]<>" in "<>str2Name];
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		]&/@dups;
	
		comp=Complement[l1[[All,1]],l2[[All,1]]];
		If[Length@comp=!=0,
			Print["The following elements from "<>str1Name<> " were not found in "<>str2Name];
			Print@comp;
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		];
	
		comp=Complement[l2[[All,1]],l1[[All,1]]];
		If[Length@comp=!=0,
			Print["The following elements from "<>str2Name<> " were not found in "<>str1Name];
			Print@comp;
			Print["Aborting in " <> ToString[funcName]];
			Abort[]
		]
	]
]


def@
MakeKeysStrings[syms_List]:=Last@StringSplit[ToString[#[[1]]],"`"]->#[[2]]&/@syms


testDef@
Aborting[fn_Symbol:$CurrentFunction]:=
Module[{},
	Print["Aborting in ", fn, "[]."];
	Abort[]
]


End[];

EndPackage[];
