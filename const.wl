(* ::Package:: *)

(*tmax=20;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];*)


tminExp=-2;
tmaxExp=2;
tmax=10.^tmaxExp;
steps=1000;
tExps=Range[tminExp,tmaxExp,(tmaxExp-tminExp)/(steps-1)];
times=10.^#&/@tExps;


runs=10;


(*length=12;*)


(*initspin={2,2,2,2,2,1,2,1,1,1,1,1};*)


length=18;


fisp=3;


(*initspin={1,1,1,1,2,2,2,2};*)


(*initspin={2,2,2,2,2,1,2,1,1,1,1,1};*)


initspin=Table[(1-(-1)^n)/2+1,{n,length}];


(*bsites=Range[1,length,2];*)


dis=4;


randfields=2 RandomReal[{-dis,dis},length];


(*diffs=Table[1/Abs[(randfields[[n]]-randfields[[n+1]])],{n,length-1}];
bsites={};
While[diffs!=Table[0,{length-1}],
max=Position[diffs,Max[diffs]][[1,1]];
AppendTo[bsites,max];
diffs[[max]]=0;
If[max!=length-1,diffs[[max+1]]=0];
If[max!=1,diffs[[max-1]]=0];
]*)


bsites=Range[1,length,2];


numbvars=Length[bsites];


ssites=Complement[Range[length],bsites,addl/@(bsites+1)];


j=4;


(*randfields=fieldsW5;*)


(*randfields={-1,0};*)


(*randfields=.5{-1,-1,1,1};*)


(*j[1]=1;
j[2]=2;
j[3]=0.9j[2];
j[4]=0;
\[CapitalDelta][1]=-1;
\[CapitalDelta][2]=-1;
\[CapitalDelta][3]=-1;
\[CapitalDelta]\.08[4]=-1;*)


(*Do[j[n]=coup[[n]];\[CapitalDelta][n]=delta[[n]];,{n,length}]*)


(*\[Alpha]=2.5;*)
