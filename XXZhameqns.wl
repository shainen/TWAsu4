(* ::Package:: *)

sscoup[ss_]:=-j[ss](cS[addl[ss]][1][t]cS[addl[ss+1]][1][t]+\[CapitalDelta][ss]cS[addl[ss]][2][t]cS[addl[ss+1]][2][t]+cS[addl[ss]][3][t]cS[addl[ss+1]][3][t])


bcoup[ss_]:=-j[ss](cB[addl[ss]][1,1][t]+\[CapitalDelta][ss]cB[addl[ss]][2,2][t]+cB[addl[ss]][3,3][t])


hamcoupsu4[ss_]:=If[MemberQ[bsites,addl[ss]],bcoup[addl[ss]],sscoup[addl[ss]]]


hamcoupsu2[ss_]:=sscoup[addl[ss]]


eqss4=Table[cS[addl[ss]][sp]'[t]==sdot[addl[ss],sp,hamcoupsu4[addl[ss-1]]+hamcoupsu4[addl[ss]]],{ss,length},{sp,3}];


eqsb4=Table[cB[addl[ss]][sp1,sp2]'[t]==bdot[addl[ss],sp1,sp2,hamcoupsu4[addl[ss]]+hamcoupsu4[addl[ss-1]]+hamcoupsu4[addl[ss+1]]],{ss,bsites},{sp1,3},{sp2,3}];


eqall4=Flatten[{eqss4,eqsb4}];


eqss2=Table[cS[addl[ss]][sp]'[t]==sdot[addl[ss],sp,hamcoupsu2[addl[ss-1]]+hamcoupsu2[addl[ss]]],{ss,length},{sp,3}];


eqall2=Flatten[{eqss2}];
