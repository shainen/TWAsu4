(* ::Package:: *)

(* ::Subsubsection:: *)
(*sz state init*)


randSpin[ss_,sp_]:=If[sp==3,3/2-initspin[[ss]],RandomChoice[{-1/2,1/2}]]


initsS := Table[cS[addl[ss]][sp][0] == randSpin[ss,sp], {ss, length}, {sp, 3}]


initsSforB := Table[cS[addl[ss]][sp][0] == randSpin[ss,sp], {ss, ssites}, {sp, 3}]


initsB:=
Table[
rspins={Table[randSpin[ss,sp],{sp,3}],Table[randSpin[addl[ss+1],sp],{sp,3}]};
s1=Table[cS[addl[ss]][sp][0]==rspins[[1,sp]],{sp,3}];
s2=Table[cS[addl[ss+1]][sp][0]==rspins[[2,sp]],{sp,3}];
s3=Flatten[Table[cB[addl[ss]][sp1,sp2][0]==rspins[[1,sp1]]rspins[[2,sp2]],{sp1,3},{sp2,3}]];
{s1,s2,s3}
,{ss,bsites}]
