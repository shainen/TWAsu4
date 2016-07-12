(* ::Package:: *)

(*hamself[ss_]:=randfields[[addl[ss]]]cS[addl[ss]][fisp][t]*)


sscoup[ss_]:=4 Sum[Abs[ss-addl[ss+nn]]^-\[Alpha] (cS[addl[ss]][1][t]cS[addl[ss+nn]][1][t]+cS[addl[ss]][2][t]cS[addl[ss+nn]][2][t]+cS[addl[ss]][3][t]cS[addl[ss+nn]][3][t]),{nn,length-1}]


(*bcoup[ss_]:=j(cB[addl[ss]][1,1][t]+cB[addl[ss]][2,2][t]+cB[addl[ss]][3,3][t])*)


(*hamcoupsu4[ss_]:=If[MemberQ[bsites,addl[ss]],bcoup[addl[ss]],sscoup[addl[ss]]]*)


hamcoupsu2[ss_]:=sscoup[addl[ss]]


(*eqss4=Table[cS[addl[ss]][sp]'[t]==sdot[addl[ss],sp,hamself[addl[ss]]+hamcoupsu4[addl[ss-1]]+hamcoupsu4[addl[ss]]],{ss,length},{sp,3}];*)


(*eqsb4=Table[cB[addl[ss]][sp1,sp2]'[t]==bdot[addl[ss],sp1,sp2,hamself[addl[ss]]+hamself[addl[ss+1]]+hamcoupsu4[addl[ss]]+hamcoupsu4[addl[ss-1]]+hamcoupsu4[addl[ss+1]]],{ss,bsites},{sp1,3},{sp2,3}];*)


(*eqall4=Flatten[{eqss4,eqsb4}];*)


eqss2=Table[cS[addl[ss]][sp]'[t]==sdot[addl[ss],sp,hamcoupsu2[addl[ss]]],{ss,length},{sp,3}];


eqall2=Flatten[{eqss2}];
