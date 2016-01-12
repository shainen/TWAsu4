(* ::Package:: *)

(* ::Subsection:: *)
(*Inits*)


random[mean_,var_]:=If[var!=0,RandomVariate[NormalDistribution[mean,Sqrt[var]]],mean]


random[mean_,var_]:=If[var!=0,RandomVariate[NormalDistribution[mean,var]],mean]


(*random[mean_,var_]:=mean*)


(* ::Subsubsection:: *)
(*spins*)


mean[op_,vec_]:=vec\[Conjugate].op.vec


cov[op1_,op2_,vec_]:=vec\[Conjugate].(op1.op2+op2.op1).vec/2-vec\[Conjugate].op1.vec vec\[Conjugate].op2.vec


spinmean=Table[mean[PauliMatrix[sp]/2,#],{sp,3}]&/@{{1,0},{0,1}};


spincov=Table[cov[PauliMatrix[sp1]/2,PauliMatrix[sp2]/2,#],{sp1,3},{sp2,3}]&/@{{1,0},{0,1}};


initsSforB:=Table[cS[addl[ss]][sp][0]==random[spinmean[[initspin[[addl[ss]]],sp]],spincov[[initspin[[addl[ss]]],sp,sp]]],{ss,ssites},{sp,3}]


initsS:=Table[cS[addl[ss]][sp][0]==random[spinmean[[initspin[[addl[ss]]],sp]],spincov[[initspin[[addl[ss]]],sp,sp]]],{ss,length},{sp,3}]


initsSingleSpin[rr_] := {}


(* ::Subsubsection:: *)
(*bispins*)


bmat[s1_,s2_]:=KroneckerProduct[PauliMatrix[s1],PauliMatrix[s2]]/If[s1==0||s2==0,2,4]


vecud[s1_,s2_]:=Flatten[Normal[KroneckerProduct[SparseArray[{initspin[[addl[s1]]]}->1,{2}],SparseArray[{initspin[[addl[s2]]]}->1,{2}]]]]


su4matrices=Join[bmat[#,0]&/@Range[3],bmat[0,#]&/@Range[3],Flatten[Outer[bmat,Range[3],Range[3]],1]];


su4varnames[ss_]:=Flatten[{cS[addl[ss]]/@Range[3],cS[addl[ss+1]]/@Range[3],Outer[cB[addl[ss]],Range[3],Range[3]]}]


matmean[op_,vec_]:=vec\[Conjugate].op.vec


bimean[ss_]:=matmean[#,vecud[addl[ss],addl[ss+1]]]&/@su4matrices


matcov[op1_,op2_,vec_]:=vec\[Conjugate].(op1.op2+op2.op1).vec/2-vec\[Conjugate].op1.vec vec\[Conjugate].op2.vec


covmat[ss_]:=Outer[matcov[#1,#2,vecud[addl[ss],addl[ss+1]]]&,su4matrices,su4matrices,1]


rotmat=Normalize/@Eigenvectors[covmat[addl[#]]]&/@Range[length];


rotcov=Eigenvalues[covmat[addl[#]]]&/@Range[length];


rotmean=Table[rotmat[[ss]].bimean[addl[ss]],{ss,length}];


initsB:=(
initsrot=Table[random[rotmean[[ss,bv]],rotcov[[ss,bv]]],{ss,length},{bv,15}];
initsorigbasis=MapThread[Dot,{Transpose[rotmat,{1,3,2}],initsrot},1];
Table[su4varnames[addl[ss]][[bi]][0]==initsorigbasis[[ss,bi]],{ss,bsites},{bi,15}]
)


initsBiSpin[rr_] := initsB
