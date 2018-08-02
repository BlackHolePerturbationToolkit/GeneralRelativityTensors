(* ::Package:: *)

<< GeneralRelativityTensors`;
<< ApplicationTools`;

packages =
{ 
  "CommonTensors",
  "TensorDefinitions",
  "TensorDerivatives",
  "TensorManipulation",
  "Utils"
};

packageSymbols = Map[# -> DocumentedSymbols["GeneralRelativityTensors", #] &, packages];

appSymbols = "GeneralRelativityTensors" -> DocumentedSymbols["GeneralRelativityTensors"];
$PackageSymbols = Join[packageSymbols, {appSymbols}]; (* Used in the Overview.md file *)

undocumentedSymbols = Map[# -> UndocumentedSymbols["GeneralRelativityTensors", #] &, packages] /. (_ -> {}) -> Sequence[]

Map[Print["Undocumented symbols for package "<>#[[1]]<>" skipped:\n", #[[2]]]&, undocumentedSymbols];

Print["Building symbol reference pages"];
docPackage[package_ -> symbols_] :=
  Map[(Print[#]; BuildSymbolReference["GeneralRelativityTensors", #, "Source"]) &, symbols];
Scan[docPackage, packageSymbols];
docPackage[appSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Building tutorials"];
tutorialSources = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Tutorials"}], Infinity];
Map[(Print[#]; BuildTutorial[FileNameJoin[{Directory[], #}]])&, tutorialSources];

Print["Indexing Documentation"];
BuildIndex["GeneralRelativityTensors"];

Print["Done"];
