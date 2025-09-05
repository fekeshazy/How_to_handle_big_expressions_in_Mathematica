(* ::Package:: *)

(* ::Section:: *)
(*GatherByDependency*)


(* ::Subsection::Closed:: *)
(*Helper functions*)


(* ::Input::Initialization:: *)
ClearAll[Dependent]

                  (*If the expression is free of the variable give back the expression.*)
         Dependent[expr_,var_]:={expr,1}/;FreeQ[expr,var]
				
				(*
                           -expanding of expression,
                           -making it a list; edgecase when we only have a multiplication not a sum
                           -separate the variable dependent terms.
                *)
         Dependent[expr_,var_]:=Block[
                  {
                  tmp=expr
                  },

                 tmp=tmp//Expand[#,var]&;
                 tmp=If[Head[tmp]===Plus,List@@tmp,{tmp}];
                 tmp=SeparateDependency[#,var]&/@tmp

         ]
				(*
                  This function separates the dependent part of an expression. This only works on single expression! Thus,
                  expressions without any addition in the numerator and expressions whichs' head is not Plus!

                  The first 4 rules are for the special cases, when Select cannot be used. Like when:
                           1. the expression is only one term;
                           2. the expression is a fraction, with one term in the numerator and one in the denominator.
                *)

         ClearAll[SeparateDependency]

                  (*The expression is a special case and is free of the variable.*)
         SeparateDependency[expr_,var_]:={expr,1}/;Head[expr]=!=Times&&FreeQ[expr,var]
         SeparateDependency[expr_,var_]:={expr,1}/;Length[expr]===0&&FreeQ[expr,var]
         SeparateDependency[expr_,var_]:={expr,1}/;Length[expr]===2&&Head[expr]===Power&&FreeQ[expr,var]

                  (*The expression is a special case and is not free of the variable.*)
         SeparateDependency[expr_,var_]:={1,expr}/;Head[expr]=!=Times&&!FreeQ[expr,var]
         SeparateDependency[expr_,var_]:={1,expr}/;Length[expr]===0&&!FreeQ[expr,var]
         SeparateDependency[expr_,var_]:={1,expr}/;Length[expr]===2&&Head[expr]===Power&&!FreeQ[expr,var]

                  (*The expression is a multiplication.*)
         SeparateDependency[expr_,var_]:=expr//{#//Select[#,FreeQ[#,var]&]&,#//Select[#,!FreeQ[#,var]&]&}&


(* ::Subsection::Closed:: *)
(*Main*)


	ClearAll[GatherByDependency]

				(*
                  If the function is free of the variable, then the function, which ought to be applied on the independent part, 
                  should be applied on the whole expression.
                *)
         GatherByDependency[
                  expr_,
                  var_Symbol | var_And | var_Or | var_Alternatives | var_Pattern, 
                  ApplyFunctionOnIndependent_: None, 
                  ApplyFunctionOnDependent_ : None]:=
                          If[ApplyFunctionOnIndependent===None,

                                  expr,
                                  expr//ApplyFunctionOnIndependent
                          ]/;FreeQ[expr,var]

                  (*
                  If we have an expression, which is a sum or a multiplication, then we can separate the independent parts.
                  *)
         GatherByDependency[
                  expr_Plus|expr_Times,
                  var_Symbol | var_And | var_Or | var_Alternatives | var_Pattern,
                  ApplyFunctionOnIndependent_: None, 
                  ApplyFunctionOnDependent_ : None]:=
         Block[
                  {
                  tmp=expr,
                  tmpFreeOfVar,
                  tmpNotFreeOfVar
                  },


                           (*Expanding the whole expression to get every dependency explicit.*)
                 tmp=tmp//Expand[#,var]&;

                           (*
                           It can happen in some cases, that due to the expansion and sorting the expression evaluates to 0.
                           In this case we have to return 0; for this we can use the second argument of Return.                           
                           *)
                 If[tmp===0, Return[tmp,Block] ];


                           (*
                           If we have only one term, then it is the easiest to quickly separate the term with Select and
                           apply the appropiate function on the appropiate parts of the expression.
                           *)
                 If[Head[tmp]=!=Plus,
				         
                                    (*Separation.*)
                           If[Length[tmp]===0,
			         
			         tmpFreeOfVar=If[FreeQ[tmp,var], tmp, 1];
				tmpNotFreeOfVar=If[!FreeQ[tmp,var], tmp, 1];,
				         
		                  tmpFreeOfVar=tmp//Select[#, FreeQ[#,var]& ]&;
		                  tmpNotFreeOfVar=tmp//Select[#, !FreeQ[#,var]& ]&;

                           ]
		         
                                    (*Applying the function(s).*)
                          Switch[
                                          {
                                          ApplyFunctionOnIndependent,
                                          ApplyFunctionOnDependent
                                          },

                                          {None,None}, tmp=tmpFreeOfVar*tmpNotFreeOfVar;,
                                          {_,None},    tmp=(tmpFreeOfVar//ApplyFunctionOnIndependent)*tmpNotFreeOfVar;,
                                          {None,_},    tmp=tmpFreeOfVar*(tmpNotFreeOfVar//ApplyFunctionOnDependent);,
                                          {_,_},       tmp=(tmpFreeOfVar//ApplyFunctionOnIndependent)*(tmpNotFreeOfVar//ApplyFunctionOnDependent);
                                  ];

                                    (*Retunr value.*)
                          tmp,


                           (*Separation.*)
                 tmp=tmp//Dependent[#,var]&;

                           (*If something goes south just return the expression*)
                 If[Head[tmp]=!=List,

                         tmp,


                                    (*Gathering by dependencies.*)
                         tmp=tmp//GatherBy[#,Last]&;

                                    (*Applying the function(s).*)
                         Switch[
                                 {
                                 ApplyFunctionOnIndependent,
                                 ApplyFunctionOnDependent
                                 },
                                 {None,None}, tmp=Flatten[{#[[All,1]]//Total,#[[1,2]]}]&/@tmp;,
                                 {_,None},    tmp=Flatten[{#[[All,1]]//Total//ApplyFunctionOnIndependent,#[[1,2]]}]&/@tmp;,
                                 {None,_},    tmp=Flatten[{#[[All,1]]//Total,#[[1,2]]//ApplyFunctionOnDependent}]&/@tmp;,
                                 {_,_},       tmp=Flatten[{#[[All,1]]//Total//ApplyFunctionOnIndependent,#[[1,2]]//ApplyFunctionOnDependent}]&/@tmp;
                         ];

                                    (*Putting everything back together.*)
                         Plus@@Times@@@tmp
                 ]

                 ]

         ]

                  (*
                  If no rule caught the function call, then apply the ApplyFunctionOnDependent function on the dependent part.
                  *)
         GatherByDependency[
                  expr_, 
                  var_Symbol | var_And | var_Or | var_Alternatives | var_Pattern, 
                  ApplyFunctionOnIndependent_: None, 
                  ApplyFunctionOnDependent_ : None]:=
                          If[ApplyFunctionOnDependent===None,

                                  expr,
                                  expr//ApplyFunctionOnDependent
                          ]


(* ::Section:: *)
(*MakeCoefficientsSymbolic*)


(* ::Subsection::Closed:: *)
(*Main*)


         (*
         This function takes an expression and a variable and returns the expression, where the coefficients of the variable dependent 
         parts are symbolic.

                  1) The first argument is the expression itself.
                  2) The secound argument is the variable, which must be a symbol.
                  3) The third argument is the dummy function/symbol head, which also must be a symbol. (Unique is a useful function.)
         *)

ClearAll[MakeCoefficientsSymbolic]


         (*If the expression is free of the variable we store the whole thing is a symbol.*)
MakeCoefficientsSymbolic[
                expr_,
                var_Symbol,
                dummyFunction_Symbol
                                                ]:= {dummyFunction[1], {dummyFunction[1]->expr}}/;FreeQ[expr,var]
                                                
         (*If the expression is a monomial we store its coefficient in a symbol.*)
MakeCoefficientsSymbolic[
                c_. var_Symbol^pow_.,
                var_Symbol,
                dummyFunction_Symbol
                                                ]:= {dummyFunction[1] var^pow, {dummyFunction[1]->c}}
                                                
         (*
         -If the expression is not an edge-case we use GatherByDependency and its second argument.
         -Account for the edge case, when we have a multiplication and not a sum.
         -Construct the rules.
         *)
MakeCoefficientsSymbolic[
                expr_,
                var_Symbol,
                dummyFunction_Symbol
                                                ]:=
         Block[
                  {
                  tmp=expr,
                  rules
                  },

                  tmp=tmp//GatherByDependency[#,var,dummyFunction]&;
                
                  If[tmp//FreeQ[#,var]&, Return[{dummyFunction[1], {dummyFunction[1]->tmp}}, Block] ];
                
                  tmp=If[Head[tmp]===Plus,List@@tmp,{tmp}];

                  rules=Table[tmp[[i]]/.c_. dummyFunction[a_]:>Rule[dummyFunction[i],a],{i,1,Length[tmp]}];
                  tmp=Table[tmp[[i]]/.c_. dummyFunction[a_]:>c dummyFunction[i],{i,1,Length[tmp]}];

                  {Plus@@tmp,rules}
         ]
         
MakeCoefficientsSymbolic[
                HoldPattern[ SeriesData[var_Symbol, point_, coeffs_List, minOrder_Integer, maxOrder_Integer, stepOrder_Integer] ],
                dummyFunction_
                                                ]:=
        Block[
                {
                },


                {
                SeriesData[var, point, Table[dummyFunction[i], {i,1,Length[coeffs]}], minOrder, maxOrder, stepOrder],

                Table[ dummyFunction[i]->coeffs[[i]], {i,1,Length[coeffs]}]
                }

        ]


(* ::Section:: *)
(*EucledianMethodPartialFraction*)


(* ::Subsection::Closed:: *)
(*Helper functions*)


		(*
		The Exponent function only gives the highest order of an expression. 
		I needed the exponent of each multiplicative term to determine the multiplicites of the denominators.
		*)
		
		(*
		-If the expression is free of the variable give back the expression itself.
		-If we have the desired structure give the expression and its power.
		
		The constant before the structure is not needed.
		*)
		
ClearAll[GetExponent]

GetExponent[list_List,var_Symbol]:=GetExponent[#,var]&/@list
GetExponent[a_. expr_^n_.,var_]:={expr^n,1}/;FreeQ[expr,var]
GetExponent[a_. expr_^n_.,var_]:={expr,n}/;!FreeQ[expr,var]


ClearAll[CheckNumericallyIfZero]
SetAttributes[CheckNumericallyIfZero, Listable]

CheckNumericallyIfZero[expr_]:=
	Block[
	{
	tmp,vars,ruleVars
	},
	
		vars=expr//Variables;
		ruleVars=Table[vars[[i]]->RandomPrime[10^6],{i,1,Length[vars]}]//Dispatch;
		
		tmp=expr/.ruleVars;
		
		If[tmp===0,
		
			0,
			expr
		]
	
	]


(* ::Subsection::Closed:: *)
(*Main*)


         (*
         This function implement the Euclidean method for partial function decomposition.

                  -The first argument is the expression, which's numerator should be only a monomial.
                  -The second argument is the variable. 
         *)

ClearAll[EuclideanMethodPartialFraction]

EuclideanMethodPartialFraction[expr_, var_, options: OptionsPattern[] ] :=expr/;FreeQ[expr,var];

EuclideanMethodPartialFraction[expr_, var_, options: OptionsPattern[] ] :=
 	Block[
  	{
   	tmp, (*Universal temporary variable.*)
   	dens, pairs, (*Veriables for denominators.*)

         rulesGCD, (*Stores the coefficients from Belzout's identitiy*)
         dummyRulesGCD, (*Stores the coefficients with symbolic coefficients.*)

         tmpRules, (*Stores the values of the coefficients.*)

   	tmp1, tmp2, tmp1Rules, tmp2Rules, (*Temporary variabels for GCD.*)

   	pow1, pow2, coeff (*Just to make sure the rules are construceted properly.*)
   	},

  
  
                           (*
                           Dissambling the expression and getting out the valuable information, like denominatos and their multiplicity.
                           *)

                  (*Getting the denominator, and making it a list.*)
  	tmp = Denominator[expr];
  	tmp = If[Head[tmp] === Power, {tmp}, List @@ tmp];
  	
  	
                  (*
                  Get each denominator with its multiplicity, plus information are the multiplicities
                  we do not need them right now.
                  *)
  	tmp = GetExponent[#, var] & /@ List @@ tmp;
  	dens = tmp[[All, 1]];
  
  
                  (*Getting all subsets of length 2 of the denominators*)
  	pairs = Subsets[dens, {2}];
  

  

                           (*
                           Getting the coefficients from Belzout's identitiy, and making them basically symbolic.
                           *)

         tmp = Table[ 
    
                  tmp = PolynomialExtendedGCD[pairs[[i, 1]], pairs[[i, 2]], var][[2]];
    
                  {tmp1, tmp1Rules} = tmp[[1]] // MakeCoefficientsSymbolic[#, var, Unique[dummyF]] &;
                  {tmp2, tmp2Rules} = tmp[[2]] // MakeCoefficientsSymbolic[#, var, Unique[dummyF]] &;
    
                  {
                  {pairs[[i, 1]] tmp1, pairs[[i, 2]] tmp2}, 
                  Flatten[{tmp1Rules, tmp2Rules}]
                  },
    
                  {i, 1, Length[pairs]}
         ];



                                    (*
                                    Making the substitution rules for the denominators.
                                    *)

                  (*Storing the rules and the coefficient rules in variabels.*)
  	{rulesGCD, tmpRules} = {tmp[[All, 1]], Flatten[tmp[[All, 2]]]};
  

  
                           (*
                           In order to make the rules automatically we have to use the With enviroment to inject both side.
                           *)
  	dummyRulesGCD = MapThread[
     
     				With[
       				{
                                             (*Temporary variables for denominator 1.*)
        				tmp11 = #1[[1]],
        				tmp12 = #1[[2]],
        
                                             (*Temporary variables for denominator 2.*)
        				tmp21 = #2[[1]],
        				tmp22 = #2[[2]]
        				},
       

                                             (*
                                             If we excplictly write out:

                                                      coeff/f/g -> coeff*a/g+coeff*b/f,

                                             where we used Belzout's identity, then we can spare the expanding.
                                             *)
                                    RuleDelayed[
                                             
                                             coeff_. Times[tmp11^pow1_, tmp12^pow2_] /; (pow1 < 0 && pow2 < 0),
                                   		coeff*tmp21*tmp11^pow1*tmp12^pow2 + coeff*tmp22*tmp11^pow1*tmp12^pow2
        	
        					]
       
       				]&,
              		         {pairs, rulesGCD}
     			
                           ]//Dispatch;


                                    (*
                                    Iterative substitution of the rules.
                                    *)
  	tmp= expr//.dummyRulesGCD;
  
  
           	(*
                  The algorithm does not grantie, that all fo the structures are gonna be reduced to their most simpler form, 
                  namely the x^pow1/denom^pow2 kind of structures can be reduced further, either by polynomial division or any other
                  methods. According to my benckmarks Apart does it quite fast.

                  This is why it was so important, to keep every coefficient symbolic, so the expansion at this point will
                  go smoothly.
           	*)
  
  	tmp = tmp // GatherByDependency[#, var, None, Apart[#, var] &] &;


                  (*Since the proper fraction has no polynomial part, we set them to 0.*)
  	tmp = tmp // GatherByDependency[#, var, None, If[PolynomialQ[#,var], 0, #]& ] &;


                  (*Subtituteing back the coefficients.*)
  	tmp=tmp/.tmpRules
  	]


(* ::Section:: *)
(*ComputeParallel*)


(* ::Subsection::Closed:: *)
(*Helper functions*)


ClearAll[ReportTime]

ReportTime[label_String, lastTime_?NumericQ] :=
 Block[
 {
 currentTime, 
 elapsed
 },
 
  currentTime = AbsoluteTime[];
  elapsed = currentTime - lastTime;
  Print[label, ": ", NumberForm[elapsed, {5, 4}], " seconds"];
  
  currentTime
]


(* ::Subsection::Closed:: *)
(*Main*)


         (*
         This function is meant to cut the overhead of the initialiaztion of the queue in parallel evaluations.

                  -It takes a list as first argument.
                  -Applies the second argment on the elements of the list. The funciton must have the head Funcition or Symbol.
         *)

ClearAll[ComputeParallel]

ComputeParallel[list_List, function_Function | function_Symbol] :=
 
 Block[
         {
         tmp, (*temporary variable*)
         submit, (*list of EvaluateObjects*)
         tmpTiming, (*temporaray variable for timing*)
         tmpJobNumber (*temporaray variable for process tracking*)
         },
  
  
                  (*
                  We have to share the varibale in order to keep track of the number of jobs across subkernels.
                  Initial value is the length of the list.
                  *)
         SetSharedVariable[tmpJobNumber];
         tmpJobNumber = Length[list];
  
  
          (*
         To launch prallel processes, first we must construct the EvaluateObjects, which can be sent to the subkernels.
    
                  -The construction is done by the ParalleSubmit funciton, 
                  while the sending and waiting is by the WaitAll funciton.
                  -The way to do it is descriped in the section Concurrency: 
                  Managing Parallel Processes at:
  
                                  https://reference.wolfram.com/language/ParallelTools/tutorial/Overview.html .
    
         -Note: related issue:
    
                  https://mathematica.stackexchange.com/questions/108223/customized-paralleltable-automate-parallelsubmit-possibly-an-issue-with/108250 .
    
          -Technical Note: 
  
                  the ParallelSumbit functions argument has the attribute HoldComplete, thus if we use a simple table the substitution will 
                  not happen. Either we put the ParallelSubmit and our funciton on Hold:
    
                           o  Table[ i//Hold[(#//f//ParallelSubmit)&]//ReleaseHold, {i, 4}]
                           o  Table[i//Inactivate[#//f//ParallelSubmit]&//Activate, {i, 4}]
    
                  or just use the appropiate built-in funciton Composition or the trick with With.
          *)
  
          submit = Table[
                         	i // Hold[
                                       (
                                         # // (
                                             Print["Calculation started."];
             
                                             {tmpTiming, tmp} = # // function // AbsoluteTiming;
             
                                                           
                                             Print["Calculation finished in: " <> ToString[ tmpTiming ] <>
                                                      " s; remaining jobs: " <> ToString[tmpJobNumber--] <> 
                                                      "."
                                             ];
                                    
                                             tmp
                                             )&//ParallelSubmit
                                       ) &
                                    ]//ReleaseHold,
    
                           {i, list}
                           ];
  
         Print["Number of jobs: " <> ToString[ Length[submit] ]];  
                  
                  (*If something went wrong abort the calculation.*)
         If[ Length[submit] != Length[list], Abort[] ];
         
                  (*Submitting jobs for evaluuation.*)
         WaitAll[submit]
]

         (*
         This version ought to decreas the initial and final overhead in parallelization computation. It takes the following arguments:

                  1) the list of subexpressions,
                  2) the function, which will be applied to the elemnts. Must have the head Funciton or Symbol,
                  3) the path of the temporary files. This path can be the path to memory in Linux or iOS systems.
         *)


ComputeParallel[expr_List, function_Function | function_Symbol, $PATHTMP_String] :=
Block[
         {
         tmp, (*Unversal temporary variable.*)

         list = expr, lengthList = Length[expr], (*Values from the expresison.*)

         tmpTiming, (*Timing variable for the actual calculation*)

         tmpFolderName, (*Variable for the temporary files.*)
         tmpJobNumber, (*Variable to track remaining jobs*)

         results, (*Return variable*)
   
   	startTime, tmpTime (*Variables for other timings.*)
         },
  
  
                           (*Writing the expression to file.*)
  

                  (*Generate random folder name.*)
         tmpFolderName = $PATHTMP <> 
                           "tmp" <> 
                           ToString[RandomInteger[{1, 10^10}]] <> "/";


                  (*Print for tracking.*)
         Print["Writing to file starts. Length of the list: " <> ToString[lengthList] <> "."];
  
  

                           (*If the directory exists overwrite it.*)
         If[DirectoryQ[tmpFolderName],
   
                  DeleteDirectory[tmpFolderName, DeleteContents -> True]
   
           ];
  	CreateDirectory[tmpFolderName];
  


           		(*Start of actual exporting.*)

                  (*Start of time measurement.*)                  
         startTime = AbsoluteTime[];
         Do[

                           (*Print which piece is currently being exported.*)
                  Print[
                           "Writing to file " <> ToString[i] <> "/" <> 
                           ToString[lengthList] <> "."];
                           tmp = list[[i]
                  ];
                   
                           (*Exporting.*)
                  DumpSave[tmpFolderName <> "tmp" <> ToString[i] <> ".mx", tmp];,
   
           {i, 1, lengthList}
         ];

                  (*Print exporting time.*)
         tmpTime = ReportTime["Exporting is done in" , startTime];
  
  


                           (*Distributing tasks to the subkernels*)
  
                  (*Clear temporary variables and share job counting variable across kernels.*)
         Clear[tmp, tmpJobNumber];
         tmpJobNumber = lengthList;
         SetSharedVariable[tmpJobNumber];
  
  
                  (*Distributing the task to the subkernels.*)
         ParallelDo[
   
                           (*Print start up message.*)
                  Print[ToString[$KernelID] <> ": Calculation started."];
            
                           (*Import element of the original list.*)
                  Import[tmpFolderName <> "/tmp" <> ToString[i] <> ".mx"];

                           (*Apply function and timing.*)
                  {tmpTiming, tmp} = tmp//function//AbsoluteTiming;
            

                           (*Print measured time of the calculation.*)
                  Print[
                           ToString[$KernelID] <> 
                           ": Calculation finished in: " <> 
                           ToString[ tmpTiming ] <>
                           "; remaining jobs: " <> 
                           ToString[tmpJobNumber--] <> "."
                  ];
            
                            (*Save result to file.*)
                  DumpSave[tmpFolderName <> "/tmp" <> ToString[i] <> ".mx", tmp];
   
   
                  , {i, 1, lengthList}

         , Method -> "FinestGrained",
         ProgressReporting -> False

         ];



                  (*Clear temporary variable just to be safe.*)
         Clear[tmp];

                  (*Reset clock*)
         startTime = AbsoluteTime[];
          
                  (*Importing back the results with the main kernel.*)
         results = Table[
                           Import[tmpFolderName <> "tmp" <> ToString[i] <> ".mx"];
                           Print[ToString[i] <> "/" <> ToString[lengthList]];
                            
                           tmp,
    
                           {i, 1, lengthList}
                  ];
  
                  (*Print importing time.*)
         tmpTime = ReportTime["Importing is done in" , startTime];

                  (*Deleting temporary directory.*)
         DeleteDirectory[tmpFolderName, DeleteContents -> True];

                  (*Print deletion time. Can be significant if the files are big.*)  
         tmpTime = ReportTime["Deleting directory is done in" , tmpTime];
  	
                  (*Return results.*)
         results
]
