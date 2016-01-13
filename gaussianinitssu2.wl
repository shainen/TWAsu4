(* ::Package:: *)

(* ::Subsection:: *)
(*Inits*)


random[mean_,var_]:=If[var!=0,RandomVariate[NormalDistribution[mean,Sqrt[var]]],mean]


(* ::Subsubsection:: *)
(*spins*)


mean[op_,vec_]:=vec\[Conjugate].op.vec


cov[op1_,op2_,vec_]:=vec\[Conjugate].(op1.op2+op2.op1).vec/2-vec\[Conjugate].op1.vec vec\[Conjugate].op2.vec


spinmean=Table[mean[PauliMatrix[sp]/2,#],{sp,3}]&/@{{1,0},{0,1}};


spincov=Table[cov[PauliMatrix[sp1]/2,PauliMatrix[sp2]/2,#],{sp1,3},{sp2,3}]&/@{{1,0},{0,1}};


initsSforB:=Table[cS[addl[ss]][sp][0]==random[spinmean[[initspin[[addl[ss]]],sp]],spincov[[initspin[[addl[ss]]],sp,sp]]],{ss,ssites},{sp,3}]


initsS:=Table[cS[addl[ss]][sp][0]==random[spinmean[[initspin[[addl[ss]]],sp]],spincov[[initspin[[addl[ss]]],sp,sp]]],{ss,length},{sp,3}]


initsSingleSpin[rr_] := initsS
