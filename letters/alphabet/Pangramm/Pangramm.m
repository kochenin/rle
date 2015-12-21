(* ::Package:: *)

BeginPackage["Pangramm`"]


IdealPangramm::usage="IdealPangramm[] \:0432\:044b\:0434\:0430\:0451\:0442 \:0438\:0434\:0435\:0430\:043b\:044c\:043d\:0443\:044e \:043f\:0430\:043d\:0433\:0440\:0430\:043c\:043c\:0443, \:043a\:043e\:0442\:043e\:0440\:0430\:044f \:0441\:043e\:0441\:0442\:043e\:0438\:0442 \:0438\:0437 \:0432\:0441\:0435\:0445 \:0431\:0443\:043a\:0432 \:0440\:0443\:0441\:0441\:043a\:043e\:0433\:043e \:0430\:043b\:0444\:0430\:0432\:0438\:0442\:0430, \:043f\:0440\:0438\:0447\:0451\:043c \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:0438 \:043f\:0440\:043e\:0431\:0435\:043b\:043e\:0432."
Pangramm::usage="Pangramm[] \:0432\:044b\:0434\:0430\:0451\:0442 \:043f\:0430\:043d\:0433\:0440\:0430\:043c\:043c\:0443 \:0438\:0437 \:0431\:0430\:0437\:044b \:0434\:0430\:043d\:043d\:044b\:0445."
PangrammInfo::usage="PangrammInfo[string] \:0432\:044b\:0434\:0430\:0451\:0442 \:0438\:043d\:0444\:043e\:0440\:043c\:0430\:0446\:0438\:044e \:043e \:0441\:0442\:0440\:043e\:043a\:0435 string \:0432 \:0432\:0438\:0434\:0435 
{\:0432\:0441\:0435 \:0437\:043d\:0430\:043a\:0438, \:0432\:0441\:0435 \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438, \:0432\:0441\:0435 \:0437\:043d\:0430\:043a\:0438 \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:0438 \:0431\:0435\:0437 \:043f\:0440\:043e\:0431\:0435\:043b\:043e\:0432, \:0432\:0441\:0435 \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438, \:0432
\:0441\:0435 \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438 \:0431\:0435\:0437 \:0443\:0447\:0451\:0442\:0430 \:0440\:0435\:0433\:0438\:0441\:0442\:0440\:0430, \:0432\:0441\:0435 \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438 \:0431\:0435\:0437 \:0443\:0447\:0451\:0442\:0430 \:0440\:0435\:0433\:0438\:0441\:0442\:0440\:0430}."


Begin["`Private`"]


Needs["RusLetters`"]


IdealPangramm[]:=StringJoin[CharacterRangeRus["\:0430","\:044f"]]


Needs["DatabaseLink`"]


connPangramm=OpenSQLConnection[JDBC["mysql","localhost/rle_letters"],"Username"->"mkletters","Password"->"10153"];


pan=SQLSelect[connPangramm,"pangramm",{"pangramm"}]


Pangramm[]:=RandomChoice[pan][[1]]


PangrammInfo[string_]:=Module[{all,allDist,allNoSign,allNoSignDist,lowerDist,lowerNoSignDist},
(*\:0412\:0441\:0435 \:0437\:043d\:0430\:043a\:0438*)
all=Length[Characters[string]];
(*\:0412\:0441\:0435 \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438*)
allDist=CountDistinct[Characters[string]];
(*\:0412\:0441\:0435 \:0437\:043d\:0430\:043a\:0438 \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:0438 \:0431\:0435\:0437 \:043f\:0440\:043e\:0431\:0435\:043b\:043e\:0432*)
allNoSign=Length[Characters[LetterString[string]]];
(*\:0412\:0441\:0435 \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438*)
allNoSignDist=CountDistinct[Characters[LetterString[string]]];
(*\:0412\:0441\:0435 \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438 \:0431\:0435\:0437 \:0443\:0447\:0451\:0442\:0430 \:0440\:0435\:0433\:0438\:0441\:0442\:0440\:0430*)
lowerDist=CountDistinct[Characters[ToLowerCaseRus[string]]];
(*\:0412\:0441\:0435 \:0431\:0435\:0437 \:0437\:043d\:0430\:043a\:043e\:0432 \:043f\:0440\:0435\:043f\:0438\:043d\:0430\:043d\:0438\:044f \:043d\:0435\:043f\:043e\:0432\:0442\:043e\:0440\:044f\:044e\:0449\:0438\:0435\:0441\:044f \:0437\:043d\:0430\:043a\:0438 \:0431\:0435\:0437 \:0443\:0447\:0451\:0442\:0430 \:0440\:0435\:0433\:0438\:0441\:0442\:0440\:0430*)
lowerNoSignDist=CountDistinct[Characters[ToLowerCaseRus[LetterString[string]]]];
{all,allNoSign,allDist,lowerDist,allNoSignDist,lowerNoSignDist}
]


PangrammQ[string_]:=Last[PangrammInfo[string]]==33


PangrammCondition[string_]:=Module[{str,alph},
str=DeleteDuplicates[Characters[ToLowerCaseRus[LetterString[string]]]];
alph=CharacterRangeRus["\:0430","\:044f"];
Complement[alph,str]
]


CloseSQLConnection[connPangramm];


End[ ]


EndPackage[ ]
