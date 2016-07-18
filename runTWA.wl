(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsection:: *)
(*Inits*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/TWAsu4"];


<<randomSeed.wl


<<modfuncs.wl


<<const.wl


(*<<QMfunc.wl*)


<<dynfunc.wl


<<runfuncs.wl


(*<<XXZhameqns.wl*)


<<heiswrfham.wl


(*<<heiswrfhamForSxStart.wl*)


(*<<hamHeis.wl;*)


(*<<gaussianinitssu2.wl*)


(*<<su2cohinits.wl*)


(*<<su4cohinits.wl*)


(*<<su4cohinitsGaussian.wl*)


(*<<XXZhameqns.wl*)


(* ::Subsection:: *)
(*run TWA*)


(*qS[ss_][sp_]:=KroneckerProduct[KroneckerProduct[IdentityMatrix[2^(ss-1)],PauliMatrix[sp]/2],IdentityMatrix[2^(length-ss)]];
hamQM=Sum[-j[ss](qS[addl[ss]][1].qS[addl[ss+1]][1]+\[CapitalDelta][ss]qS[addl[ss]][2].qS[addl[ss+1]][2]+qS[addl[ss]][3].qS[addl[ss+1]][3])(*+field[[ss]]qS[ss][fisp]*),{ss,1,length}];
QMSpin=QMSpinsFromHam[length,hamQM,times,initspin,fisp];*)


<<gaussianinitssu2.wl
TWASpGauSU2=TWASU2Spins;


<<gaussianinits.wl
TWASpGauSU4=TWASU4SpinsGau;


<<initsDiscSU2SU4uncor.wl
TWASpDiscSU2=TWASU2Disc;
(*TWASpDiscSU4=TWASU4Disc;*)


<<initsDiscSU2SU4uncor.wl
TWASpDiscSU4=TWASU4Disc;
(*TWASpDiscSU4=TWASU4Disc;*)


runs=1;
<<deltainits.wl
TWASpDelta=TWASU4Spins;


(*runs=1;
bsites={1,3};
<<XXZhameqns.wl
<<deltainits.wl
TWASpdelta13=TWASU4Spins;
bsites={2,4};
<<XXZhameqns.wl
<<deltainits.wl
TWASpdelta24=TWASU4Spins;*)


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["spinchain.dat",{mmu,randfields,TWASpDiscSU2,TWASpDiscSU4,TWASpGauSU2,TWASpGauSU4,TWASpDelta}];
