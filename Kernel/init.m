(* ::Package:: *)

(* Mathematica Init File *)


(*Get[FileNameJoin[{FileNameDrop[FindFile["SchwAnalysis`"],-2],"Stack.m"}]];
Get[FileNameJoin[{FileNameDrop[FindFile["SchwAnalysis`"],-2],"StackTrace.m"}]];
Get["SchwAnalysis`ArgumentChecker`"];
Get["SchwAnalysis`Error`"];*)
Get["Tensors`Kernel`Tensors`"];


Block[{},
	Unprotect[$Packages];
	$Packages = Complement[$Packages, Tensors`Private`packages];
	Protect[$Packages];

	Scan[Needs, Tensors`Private`packages];
]
