(* ::Package:: *)

(* ::Section:: *)
(*Load*)


(*If run in terminal, this will not work!*)
SetDirectory[NotebookDirectory[]]


(*Importing the functions from the blog post.*)
Import["Functions.m"];


(*Examples from LinApart*)
exampleSimple=Import["exampleSimple.mx"];
structures=Import["PF_structures"]//ToExpression;

(**Custom example for this blog post*)
Import["exampleSeries.mx"];


(* ::Section:: *)
(*Examples*)


(* ::Subsection::Closed:: *)
(*GatherByDependency*)


(*
exampleSimple is a smallish example compared to the expressions, which aroise in high enegry phsics calculations; but still it is a 
prime expression to demonstrate that, just by gathering terms, how much resource one can spare.
*)


(* ::Input::Initialization:: *)
(*Apart most probably applies itself on every term.*)
tmpApart=exampleSimple//Apart[#,x1]&;//MaxMemoryUsed//AbsoluteTiming


(*We can just gather by dependency and apply Apart on the unique structures, thus doing only as much work as necessary.*)
tmpGather=exampleSimple//GatherByDependency[#,x1,None,Apart[#,x1]&]&;//MaxMemoryUsed//AbsoluteTiming


(*Numerical check.*)
tmpApart-tmpGather//CheckNumericallyIfZero
tmp=(tmpApart-tmpGather//Expand);


(* ::Subsection::Closed:: *)
(*MakeCoefficientsSymbolic*)


(*
Series is an elementary function/operation in computer algebra, thus its efficiency is vital; but even when we just want the series expansion
to some order of a product of two polynomials it takes ages. But this issue is rooted in its behavoiur, thus if we apply it cleverly
we can achive great optimization.

Here, I just make the coeffcients of two "larger" polynomials symbolic; the effects are astonishing, since this simple trick
proides magintudes of speedup.
*)

{tmpSeries1,tmpSeries1Rules}=series1//MakeCoefficientsSymbolic[#,eps,Unique[dummyF]]&;
{tmpSeries2,tmpSeries2Rules}=series2//MakeCoefficientsSymbolic[#,eps,Unique[dummyF]]&;

seriesProduct1=series1*series2//Series[#,{eps,0,9}]&;//AbsoluteTiming
seriesProduct2=series1*series2//Plus[#,\!\(\*
TagBox[
InterpretationBox[
SuperscriptBox[
RowBox[{"O", "[", "eps", "]"}], "10"],
SeriesData[$CellContext`eps, 0, {}, 10, 10, 1],
Editable->False],
Short]\)]&;//AbsoluteTiming
seriesProduct3=(series1//Series[#,{eps,0,12}]&)*(series2//Series[#,{eps,0,12}]&)//Series[#,{eps,0,9}]&;//AbsoluteTiming
seriesProduct4=tmpSeries1*tmpSeries2//
							Series[#,{eps,0,9}]&//
							ReplaceAll[#, Join[tmpSeries1Rules,tmpSeries2Rules]//Dispatch]&;//AbsoluteTiming


(*NUmerical checks.*)
seriesProduct1-seriesProduct3//Short
seriesProduct1-seriesProduct4//Short


(* ::Subsection:: *)
(*EucledianMethodPartialFraction*)


(*
Mathematica uses the equation system method to calculate the partial fraction decomposition of the given fraction. This method is inheritly
unoptimal, since in this case the computer must solve a huge equation system symbolically(!). Thus, the run-time explodes with the 
number of variables and with everything, whcih influences the size of the equation system.

There are better algorithms to calculate a partial fraction decomposition, like the Euclidean method or LinApart. Here, I showcase
how the Euclidean method beats Apart. 
*)


(*Just for info.*)
structures//Length


(*Calculation of some simple structure with the Euclidean algorihm.*)
Monitor[
	timingStructuresEuclidean=Table[
		structures[[counter]]//EuclideanMethodPartialFraction[#,x2]&//AbsoluteTiming,
		{counter,3,20}
	];,
{counter,Length[structures],structures[[counter]]}]


(*Calculation of the same fractions with Apart; I must put a TimeConstrained on it.*)
maximumTime=10;

Monitor[
	timingStructuresApart=Table[
			TimeConstrained[
				Apart[structures[[counter]],x2]//AbsoluteTiming,
				maximumTime,
				{Overtime,Overtime}
			],
		{counter,3,20}
	];,
{counter,Length[structures],structures[[counter]]}]


(*We can see, that Apart scales much worse than the Euclidean algorithm.*)
MapThread[
	{#1,#2}&,
	{
	timingStructuresEuclidean[[All,1]],
	timingStructuresApart[[All,1]]
	}
]//Column


(*Checks.*)
structures[[3;;20]]-timingStructuresEuclidean[[All,2]]//CheckNumericallyIfZero
structures[[3;;20]]-timingStructuresApart[[All,2]]//CheckNumericallyIfZero


(* ::Subsection:: *)
(*ComputeParallel*)


(*Launching subkernels for parallel computations.*)
CloseKernels[];
LaunchKernels[4];
$KernelCount


(* ::Subsubsection::Closed:: *)
(*Overhead at start*)


(*"Complicated" fractions.*)
fractions=Table[ 1/Product[ Sum[b[i,j,k] x^i, {i,1,2}]^2, {j,1,8}] ,{k,1,4}];


(*
Calculation of said fractions with different methods. One can see that, while the overhead at the start is not significant, since 
the fractions are small, the overhead at the end is big, due to the size of the results.
*)

resultsSequential=fractions//Map[EuclideanMethodPartialFraction[#,x]&, #]&;//AbsoluteTiming
resultsParallelMap=fractions//ParallelMap[EuclideanMethodPartialFraction[#,x]&, #, ProgressReporting->False]&;//AbsoluteTiming

resultsParallel1=ComputeParallel[fractions, EuclideanMethodPartialFraction[#,x]&];//AbsoluteTiming
(*If one wants to run this line from the command line then the NotebookDirectory[] must be changed, since it requires a front-end.*)
resultsParallel2=ComputeParallel[fractions, EuclideanMethodPartialFraction[#,x]&, NotebookDirectory[]];//AbsoluteTiming


(resultsSequential-resultsParallel2//Total)===0


(* ::Subsubsection::Closed:: *)
(*Overhead at end*)


(*Making a list of "big" expressions.*)
tmp=series1;
tmp=Table[ tmp, {i,1,10}];


(*
Here one can see the effects of the overhead at initialization, ParallelMap takes more time than the sequential calculation!

This result is ridiculos, it shouldn't be faster to dump the expression onto disk and importing it back than using the built-in 
function. This is a huge oversite of the Q&A department, if it even exists...

Clue: if run in terminal and tracing it with debugger (Crtl-C), one can observe that a StringMatchQ is called a million times...
*)
resultsSequential=tmp//Map[Expand, #]&;//AbsoluteTiming
resultsParallelMap=tmp//ParallelMap[Expand, #, ProgressReporting->False]&;//AbsoluteTiming

resultsParallel1=ComputeParallel[tmp, Expand];//AbsoluteTiming
(*If one wants to run this line from the command line then the NotebookDirectory[] must be changed, since it requires a front-end.*)
resultsParallel2=ComputeParallel[tmp, Expand, NotebookDirectory[]];//AbsoluteTiming


(*Checks.*)
(resultsSequential-resultsParallel1//Total)===0
(resultsSequential-resultsParallel2//Total)===0
(resultsParallelMap-resultsParallel2//Total)===0



