(* ::Package:: *)

(*****************************************************	
A sample of code pulled from our MasterFunctions.m
Wolfram Language package. For full functional code, 
one should go to the MasterFunctions repo. 
At a few points, the functions dispalyed herein call
other functions from MasterFunctions.m---such as
"GetVars," which is similar to the native "Variables" 
function---as well as utilize some variables from
MasterFunctions.m such as "distributedContexts". 
However, these calls are rare and should not affect 
the flow of the code in any material way.

See "TensorReduction.pdf" for a detailed explanation.

This file is best viewed with Mathematica, but may
be viewed with a standard text editor.
******************************************************)


(* ::Section:: *)
(*Tensor Reduction*)


(* ::Subsection::Closed:: *)
(*Split by Twos*)


(***************************************************)


ClearAll[GetSBT];
GetSBT[n_Integer]:= Block[
{ran,directory,name,suffix,fileName,sbt},
	If[OddQ[n], Print["Odd number of momenta. Returning NULL..."];Return[];];

	ran = Range[n];
	(*Search if saved*)
	directory = MASTERFUNCTIONSDIRECTORY<>"aux/tensorReduction/splitByTwos/";
	name = ToString[n];
	suffix = ".m";
	fileName = directory<>name<>suffix;
	SetDirectory[NotebookDirectory[]];
	PrintQ["Loading split-by-twos info..."];
	PrintQ[AbsoluteTiming[Quiet[sbt = Get[fileName];];][[1]]];
	If[ sbt === $Failed,
		PrintQ["Can't find cached split-by-twos."];
		(*If not saved build it and save it*)
		PrintQ["Splitting by twos..."];
		PrintQ[AbsoluteTiming[sbt = SplitByTwos[ran];][[1]]];
		SetDirectory[NotebookDirectory[]];
		Quiet[DeleteFile[fileName];];
		Put[sbt,fileName];
	];
	Return[sbt];
];


(* 2-14 points take < 5s, 16-point takes ~ 1 min *)
(* From Tristan Dennen *)
ClearAll[SplitByTwos];
SplitByTwos[arg_List]:=  
	Catch[Block[
		{list=arg,rlist = Rest[arg],pairs,set},
		If[Length[list]==2,Throw[{{list}}];];
		pairs = Table[Join[{{list[[1]],list[[i]]}},#]&/@SplitByTwos[Drop[rlist,{i-1}]],{i,2,Length[list]}];
		Throw[Flatten[pairs,1]];
	set
	]];


(* ::Subsection::Closed:: *)
(*Tensor Structures that Obey Symmetries*)


(***************************************************)


(* Example: input = {3,3,2} ~ pppqqqrr *)
(* Sorts input labels so 2.3.3 & 3.3.2 aren't separately generated *)
ClearAll[GetTensorStructs];
GetTensorStructs[input_List]:=Block[
{n,ran,ids,directory,suffix,name,fileName,sbt,tens,i},
	n = Plus@@input;
	ran = Range[n];
	ids = {};
	For[i = 1, i <= Length[input], i++,
		AppendTo[ids,ran[[1;;input[[i]]]]];
		ran = Drop[ran,input[[i]]];
	];
	ran = Range[n];

	If[OddQ[n], PrintQ["Odd number of momenta. Returning NULL..."];Return[];];

	(* Basically, splitting products of \[Eta]'s into equivalence classes based on symmetry *)
	directory = MASTERFUNCTIONSDIRECTORY<>"aux/tensorReduction/tensorStructs/";
	suffix = ".m";
	name = StringJoin@@Riffle[ToString/@input,"."];
	fileName = directory<>name<>suffix;
	SetDirectory[NotebookDirectory[]];
	PrintQ["Loading tensors that obey input symmetry..."];
	PrintQ[AbsoluteTiming[Quiet[tens = Get[fileName];];][[1]]];
	If[ tens === $Failed,
		PrintQ["Can't find cached tensor structures."];
		(*If not saved build it and save it*)
		(**************************************)
		(************* SplitByTwos ***********)
		(**************************************)
		sbt = GetSBT[n];
		(**************************************)
		(**** Tensor symmetry structures *****)
		(**************************************)
		PrintQ["Building tensors (2 steps)..."];
		PrintQ[AbsoluteTiming[tens = GetIdenticalStructs[sbt, ids];][[1]]];
		PrintQ[AbsoluteTiming[tens = ParallelTable[(sbt[[tens[[i,j]]]]),{i,1,Length[tens]},{j,1,Length[tens[[i]]]},DistributedContexts :> distributedContexts];][[1]]];
		SetDirectory[NotebookDirectory[]];
		Quiet[DeleteFile[fileName];];
		Put[tens,fileName];
	];
	Return[tens];
];


ClearAll[GetIdenticalStructs];
GetIdenticalStructs[sbt_List, identifications_List] := Block[
{reps,dupPos},
	If[ identifications === {},
		Return[{}];
	];
	reps = (Sort/@identifications);
	reps = Flatten[Thread[Drop[#,1]->#[[1]]]&/@reps];

	dupPos = (Sort[Sort/@#])&/@(sbt//.reps);
	dupPos = PositionDuplicates[dupPos];
	Return[dupPos];
];


ClearAll[PositionDuplicates];
PositionDuplicates[list_] := GatherBy[Range@Length[list], list[[#]] &];


(* ::Subsection::Closed:: *)
(*Scalar Products from Tensor Structures*)


(***************************************************)


(* Each representative from a tensor structure contracts the input momenta in a unique way *)
(* Example: input = {3,3,2} ~ pppqqqrr *)
(* Input labels are sorted so 2.3.3 & 3.3.2 aren't separately generated *)
ClearAll[GetScalarProducts];
GetScalarProducts[input_List] := Block[
{n,ran,ids,directory,suffix,name,fileName,sps,reps,i},
	n = Plus@@input;
	ran = Range[n];
	ids = {};
	For[i = 1, i <= Length[input], i++,
		AppendTo[ids,ran[[1;;input[[i]]]]];
		ran = Drop[ran,input[[i]]];
	];
	ran = Range[n];

	directory = MASTERFUNCTIONSDIRECTORY<>"aux/tensorReduction/scalarProducts/";
	suffix = ".m";
	name = StringJoin@@Riffle[ToString/@input,"."];
	fileName = directory<>name<>suffix;
	SetDirectory[NotebookDirectory[]];
	PrintQ["Loading scalar products..."];
	PrintQ[AbsoluteTiming[Quiet[sps = Get[fileName];];][[1]]];
	If[ sps === $Failed,
		PrintQ["Can't find cached scalar products."];
		(**************************************)
		(**** Tensor symmetry structures *****)
		(**************************************)
		sps = GetTensorStructs[input];
		(* Only need one representative *)
		sps = sps[[All,1]];
		(**************************************)
		(********** Scalar Products **********)
		(**************************************)
		PrintQ["Building scalar products..."];
		reps = #->Position[ids,#][[1,1]]&/@Flatten[ids];
		PrintQ[AbsoluteTiming[sps = (Sort[Sort/@#])&/@(sps/.reps);][[1]]];
	
		SetDirectory[NotebookDirectory[]];
		Quiet[DeleteFile[fileName];];
		Put[sps,fileName];
	];
	Return[sps];
];


(* ::Subsection::Closed:: *)
(*Coefficients from Tensor Contractions*)


(***************************************************)


ClearAll[GetContractions];
(* Out: Sum of tens[[i]] being contracted by any representative of tens[[j]] *)
(* All representatives should give same contraction (if tens[[i]] is summed) *)
(* Example: input = {3,3,2} ~ pppqqqrr *)
(* Input labels are sorted so 2.3.3 & 3.3.2 aren't separately generated *)
GetContractions[input_List] := Block[
{directory,suffix,name,fileName,coefs,tens},

	directory = MASTERFUNCTIONSDIRECTORY<>"aux/tensorReduction/tensorContractions/";
	suffix = ".m";
	name = StringJoin@@Riffle[ToString/@input,"."];
	fileName = directory<>name<>suffix;
	SetDirectory[NotebookDirectory[]];
	PrintQ["Loading tensor contractions..."];
	PrintQ[AbsoluteTiming[Quiet[coefs = Get[fileName];];][[1]]];
	If[ coefs === $Failed,
		PrintQ["Can't find cached tensor contractions."];
		(**************************************)
		(**** Tensor symmetry structures *****)
		(**************************************)
		tens = GetTensorStructs[input];
		(**************************************)
		(****** Contraction of tensors *******)
		(**************************************)
		PrintQ["Finding all relevant contractions to build equations..."];
		PrintQ[AbsoluteTiming[
			coefs = 
				ParallelTable[
					Sum[ 
						Ds^ContractionCycles[tens[[i,a]],tens[[j,1]]]
					,{a,1,Length[tens[[i]]]}]
				,{i,1,Length[tens]},{j,1,Length[tens]}
				,DistributedContexts :> distributedContexts
				];
		][[1]]];
		SetDirectory[NotebookDirectory[]];
		Quiet[DeleteFile[fileName];];
		Put[coefs,fileName];
	];
	Return[coefs];
];


(* This is the workhorse that runs thousands or millions of times: 
		~(# of tensor structures)*(length of tensor structure) *)
(* Need speed with good constants *)
(* Problem is same as finding connected components in undirected graphs *)
(* Note: Took out error checking for speed *)
ClearAll[ContractionCycles];
ContractionCycles = Compile[{
{pairList, _Integer, 2},
{pairsToContract, _Integer, 2}},
Block[{edges,len,nodes,i,n1,n2,numCC,next,e1,e2},
	(*If[ !(Sort[Flatten[pairList]] === Sort[Flatten[pairsToContract]]),
		Print["Need complete and valid list of contractions."];
		numCC = -1;,*)

		edges = Join[pairList,pairsToContract];
		len = Length[edges];
		(* numNodes === numEdges since every vertex has 2 edges *)
		(* {outEdge1, outEdge2, visited?} *)
		nodes = ConstantArray[{0,0,0},len];
		For[i = 1, i <= len, i++,
			n1 = edges[[i,1]];
			n2 = edges[[i,2]];
			If[nodes[[n1,1]]===0, nodes[[n1,1]] = n2, nodes[[n1,2]] = n2];
			If[nodes[[n2,1]]===0, nodes[[n2,1]] = n1, nodes[[n2,2]] = n1];
		];

		(* Number of connected components *)
		numCC = 0;
		For[i = 1, i <= len, i++,
			(* If unvisited, sorta BFS *) 
			(* Go around circle since only two edges per vertex *)
			next = i;
			If[ nodes[[next,3]] === 0,
				numCC++;
				nodes[[next,3]] = 1;
				e1 = nodes[[next,1]];
				e2 = nodes[[next,2]];
				While[Or[nodes[[e1,3]]===0,nodes[[e2,3]]===0],
					next = If[nodes[[e1,3]]===0, e1, e2];
					nodes[[next,3]] = 1;
					e1 = nodes[[next,1]];
					e2 = nodes[[next,2]];
				];
			 ];
		];
	(*];*)
numCC
],
CompilationTarget -> "C",
RuntimeAttributes -> Listable
];


(*(****Over 10x's slower using Mathematica functions******)
ClearAll[Contract2];
Contract2 = Compile[{
{pairList, _Integer, 2},
{pairsToContract, _Integer, 2}},
	If[ !(Sort[Flatten[pairList]] === Sort[Flatten[pairsToContract]]),
		Print["Need complete and valid list of contractions."];
		-1,

		Length[ConnectedComponents[FromUnorderedPairs[Join[pairList,pairsToContract]]]]
	],
	RuntimeAttributes -> Listable
];
*)


(* ::Subsection::Closed:: *)
(*Equation Building*)


(***************************************************)


(* Each scalar product has a sum of tensor structures in front of it *)
(* This function generates equations for the coefficients in front of each structure for each scalar product *)
(* Number of equations and coefficients will be (# of tensor structures)^2 *)
(* Example: input = {3,3,2} ~ pppqqqrr *)
(* Input labels are sorted so 2.3.3 & 3.3.2 aren't separately generated *)
ClearAll[GetEqs];
GetEqs[input_List] := Block[
{directory,suffix,name,fileName,eqs,contractCoefs},
	directory = MASTERFUNCTIONSDIRECTORY<>"aux/tensorReduction/equations/";
	suffix = ".m";
	name = StringJoin@@Riffle[ToString/@input,"."];
	fileName = directory<>name<>suffix;
	SetDirectory[NotebookDirectory[]];
	PrintQ["Loading equations..."];
	PrintQ[AbsoluteTiming[Quiet[eqs = Get[fileName];];][[1]]];
	If[ eqs === $Failed,
		PrintQ["Can't find cached equations."];
		(**************************************)
		(******** Tensor Contractions ********)
		(**************************************)
		contractCoefs = GetContractions[input];
		(**************************************)
		(************* Equations **************)
		(**************************************)
		PrintQ["Building equations..."];
		PrintQ[AbsoluteTiming[
		eqs = Table[
				Table[
					Sum[a[j,i]*contractCoefs[[i,k]],{i,1,Length[contractCoefs]}]== KroneckerDelta[j,k],
				{j,1,Length[contractCoefs]}],
			{k,1,Length[contractCoefs]}];][[1]]];
		eqs = Transpose[eqs];
	
		SetDirectory[NotebookDirectory[]];
		Quiet[DeleteFile[fileName];];
		Put[eqs,fileName];
	];
	Return[eqs];
];


(* ::Subsection::Closed:: *)
(*Solver*)


(***************************************************)


(* Example: input = {3,3,2} ~ pppqqqrr *)
ClearAll[GetSoln];
GetSoln[input_List] := Block[
{directory,suffix,name,fileName,soln,eqs,slen,contractCoefs},
	directory = MASTERFUNCTIONSDIRECTORY<>"aux/tensorReduction/solutions/";
	suffix = ".m";
	name = StringJoin@@Riffle[ToString/@input,"."];
	fileName = directory<>name<>suffix;
	SetDirectory[NotebookDirectory[]];
	PrintQ["Loading solutions..."];
	PrintQ[AbsoluteTiming[Quiet[soln = Get[fileName];];][[1]]];
	If[ soln === $Failed,
		PrintQ["Can't find cached solutions."];
		(**************************************)
		(******** Tensor Contractions ********)
		(**************************************)
		eqs = GetEqs[input];
		(**************************************)
		(************* Equations **************)
		(**************************************)
		slen = ToString[Length[eqs]];
		PrintQ["Solving "<>slen<>"x"<>slen<>" equations..."];
		PrintQ[AbsoluteTiming[
		soln = Factor[
			ParallelTable[
				Solve[eqs[[i]],GetVars[eqs[[i,All,1]],{Ds}]][[1]],
			{i,1,Length[eqs]}
			,DistributedContexts :> distributedContexts]
		];][[1]]];
	
		SetDirectory[NotebookDirectory[]];
		Quiet[DeleteFile[fileName];];
		Put[soln,fileName];
	];
	Return[soln];
];


(* ::Subsection::Closed:: *)
(*User Interface: TensorReduce*)


(***************************************************)


(* Can handle a sum of inputs. We send each to TensorReduce0. *)
(* ClearAll[TensorReduce]; *)
TensorReduce[in_] := Block[
{tensors,vars,pres,printCell,tenCount,tenOrder},
	tensors = Expand[in];
	vars = Complement[GetVars[tensors],GetVars[tensors,{lk[_,_],le[_,_],lE[_,_]}]];
	If[vars === {},(*No tensors*)
		Return[{{tensors,1}}];
	];
	tensors = CoefficientRules[tensors,vars];
	pres = tensors[[All,2]];
	tensors = tensors[[All,1]];
	tensors = Table[
		Flatten[Table[ConstantArray[vars[[j]],tensors[[i,j]]],{j,1,Length[tensors[[i]]]}]]
	,{i,1,Length[tensors]}];
	
	tenCount = 0;
	tenOrder = 0;
	printCell = PrintTemporary[
					"Reducing tensor ",Dynamic[tenCount]," of "<>ToString[Length[tensors]],"\n",
					"Tensor length: ",Dynamic[tenOrder]
				];
	tensors = Table[
				tenCount = i;
				tenOrder = Length[tensors[[i]]];
				TensorReduce0[tensors[[i]],pres[[i]]]
			,{i,1,Length[tensors]}];
	NotebookDelete[printCell];

	Join@@tensors			
];


(* This can only handle a list of {le[5,1], lk[6,2], lk[6,2], ...} *)
ClearAll[TensorReduce0];
TensorReduce0[in_List,pre_] := Block[
{toRed = in,input,lReps,indexReps,sps,tens,soln,red},

	If[toRed === {},
		Return[{{pre,1}}];(* If no tensors...*)
	];
	toRed = GatherBy[toRed,#/.{le[i_,j_]:>i, lk[i_,j_]:>i, lE[i_,j_]:>i}&];
	toRed = Reverse[SortBy[toRed,Length]];

	input = Length/@toRed;
	If[OddQ[Plus@@input],
		Return[{{0,0}}]; (* If odd number of tensors, return 0 *)
	];

	lReps = Thread[ Range[Length[input]] -> Table[toRed[[i,1]]/.{lk[i_,j_]:>i,le[i_,j_]:>i,lE[i_,j_]:>i}, {i,1,Length[toRed]}] ];
	lReps = Dispatch[(# -> ll@@(#/.lReps)) &/@ Tuples[Range[Length[input]],2]];

	indexReps = Thread[ Range[Length[Flatten[toRed]]] -> (Flatten[toRed]/.{lk[i_,j_]:>p[j],le[i_,j_]:>\[Epsilon][j],lE[i_,j_]:>\[Epsilon]2[j]}) ];
	indexReps = Dispatch[(# -> ((AngleBracket@@(#/.indexReps))/. ZEROREPS)) &/@ Tuples[Range[Length[indexReps]],2]];

	sps = GetScalarProducts[input];
	sps = Times@@@(sps /. lReps);

	tens = GetTensorStructs[input];
	tens = Table[Plus@@(Times@@@(tens[[i]] /. indexReps)),{i,1,Length[tens]}];

	soln = GetSoln[input];
	soln = soln/.Rule[x_,y_]:>y;

	red = Table[{pre*sps[[i]], (soln.tens)[[i]]},{i,1,Length[sps]}];

	red
];
