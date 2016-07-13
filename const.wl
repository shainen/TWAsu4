(* ::Package:: *)

(*tmax=20;
steps=1000;
times=Range[0,tmax,tmax/(steps-1)];*)


tminExp=-2;
tmaxExp=1.3;
tmax=10.^tmaxExp;
steps=1000;
tExps=Range[tminExp,tmaxExp,(tmaxExp-tminExp)/(steps-1)];
times=10.^#&/@tExps;


runs=100;


(*length=12;*)


(*initspin={2,2,2,2,2,1,2,1,1,1,1,1};*)


length=25;


fisp=3;


(*initspin={1,1,1,1,2,2,2,2};*)


(*initspin={2,2,2,2,2,1,2,1,1,1,1,1};*)


initspin=Table[(1+(-1)^n)/2+1,{n,length}];


(*bsites=Range[1,length,2];*)


bsites=Range[1,length,2];


numbvars=Length[bsites];


ssites=Complement[Range[length],bsites,addl/@(bsites+1)];


(*j=1;*)


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


\[Alpha]=2.5;
