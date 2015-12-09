(* ::Package:: *)

tmax=8;
steps=500;
times=Range[0,tmax,tmax/(steps-1)];


runs=100;


length=12;


initspin={2,2,2,2,2,1,2,1,1,1,1,1};


(*initspin={2,1};*)


bsites=Range[1,length-1,2];


numbvars=Length[bsites];


ssites=Complement[Range[length],bsites,addl/@(bsites+1)];


j=-1;


randfields=fieldsW5;


(*j[1]=0.5;
j[2]=0;
j[3]=1;
j[4]=1;
\[CapitalDelta][1]=-1;
\[CapitalDelta][2]=-1;
\[CapitalDelta][3]=-1;
\[CapitalDelta][4]=-1;*)
