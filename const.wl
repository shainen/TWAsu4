(* ::Package:: *)

tmax=20;
steps=500;
times=Range[0,tmax,tmax/(steps-1)];


runs=100;


(*length=12;*)


(*initspin={2,2,2,2,2,1,2,1,1,1,1,1};*)


length=3;


fisp=3;


initspin={1,1,1};


(*bsites=Range[1,length,2];*)


bsites={1};


numbvars=Length[bsites];


ssites=Complement[Range[length],bsites,addl/@(bsites+1)];


(*j=-1;*)


(*randfields=fieldsW5;*)


(*randfields={-1,0};*)


(*randfields=.5{-1,-1,1,1};*)


j[1]=1;
j[2]=2;
j[3]=0.9j[2];
j[4]=0;
\[CapitalDelta][1]=-1;
\[CapitalDelta][2]=-1;
\[CapitalDelta][3]=-1;
\[CapitalDelta][4]=-1;
