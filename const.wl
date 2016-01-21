(* ::Package:: *)

tmax=100;
steps=500;
times=Range[0,tmax,tmax/(steps-1)];


runs=10;


(*length=12;*)


(*initspin={2,2,2,2,2,1,2,1,1,1,1,1};*)


length=12;


fisp=3;


initspin={1,1,1,1,2,2,2,2};


initspin={2,2,2,2,2,1,2,1,1,1,1,1};


(*bsites=Range[1,length,2];*)


bsites={5,7,12};


numbvars=Length[bsites];


ssites=Complement[Range[length],bsites,addl/@(bsites+1)];


j=1;


randfields=randfield2;


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
