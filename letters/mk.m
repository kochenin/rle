BeginPackage["Pangram`"]

IdealPangram::usage="IdealPangram[] выдаёт идеальную панграмму, которая состоит из всех букв русского алфавита, причём без знаков препинания и пробелов."
Pangram::usage="Pangram[] выдаёт панграмму из базы данных."
PangramInfo::usage="PangramInfo[string] выдаёт информацию о строке string в виде {Все знаки, Все неповторяющиеся знаки, Все неповторяющиеся знаки без учёта регистра, Все буквы без знаков препинания и без пробелов, Все буквы без знаков препинания и без пробелов, различные, Все буквы без знаков препинания и без пробелов, различные, без учёта регистра}."
PangramQ::usage="PangramQ[string] проверяет строку string на кандидата в панграммы."
PangramCondition::usage="PangramCondition[string] проверяет строку string и выдаёт дополнительные буквы для условия панграммы."
PangramInDBQ::usage="PangramInDBQ[string] проверяет, содержится ли строка string в базе данных."
PangramCount::usage="PangramCount[] выдаёт количество панграмм в базе данных."

(*это что то новенькое и снова*)

Begin["`Private`"]

Needs["LettersRus`"]

IdealPangram[]:=StringJoin[CharacterRangeRus["а","я"]]

Needs["DatabaseLink`"]

connPangram=OpenSQLConnection[JDBC["mysql","localhost/rle_letters"],"Username"->"user","Password"->"pass"];

pan=SQLSelect[connPangram,"pangram",{"pangram"}]

PangramInDBQ[string_]:=MemberQ[pan,string]

PangramCount[]:=Length[pan]

Pangram[]:=RandomChoice[pan][[1]]

PangramInfo[string_]:=Module[{all,allDist,allNoSign,allNoSignDist,lowerDist,lowerNoSignDist},
(*Все знаки*)
all=Length[Characters[string]];
(*Все неповторяющиеся знаки*)
allDist=CountDistinct[Characters[string]];
(*Все неповторяющиеся знаки без учёта регистра*)
lowerDist=CountDistinct[Characters[ToLowerCaseRus[string]]];

(*Все буквы без знаков препинания и без пробелов*)
allNoSign=Length[Characters[LetterString[string]]];
(*Все буквы без знаков препинания и без пробелов, различные*)
allNoSignDist=CountDistinct[Characters[LetterString[string]]];
(*Все буквы без знаков препинания и без пробелов, различные, без учёта регистра*)
lowerNoSignDist=CountDistinct[Characters[ToLowerCaseRus[LetterString[string]]]];
{all,allDist,lowerDist,allNoSign,allNoSignDist,lowerNoSignDist}
]

PangramQ[string_,exc_]:=CountDistinct[Characters[ToLowerCaseRus[LetterString[string]]]]==33-Length[exc]&&StringFreeQ[string,exc]

PangramCondition[string_]:=Module[{str,alph},
str=DeleteDuplicates[Characters[ToLowerCaseRus[LetterString[string]]]];
alph=CharacterRangeRus["а","я"];
Complement[alph,str]
]

CloseSQLConnection[connPangram];

End[ ]

EndPackage[ ]
