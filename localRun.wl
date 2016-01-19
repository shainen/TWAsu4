(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/TWAsu4"];*)


<<const.wl


<<modfuncs.wl


(*<<QMfunc.wl*)


<<dynfunc.wl


<<runfuncs.wl


<<heiswrfham.wl


(*<<XXZhameqns.wl*)


(* ::Subsection:: *)
(*run TWA*)


(*qS[ss_][sp_]:=KroneckerProduct[KroneckerProduct[IdentityMatrix[2^(ss-1)],PauliMatrix[sp]/2],IdentityMatrix[2^(length-ss)]];
hamQM=Sum[-j[ss](qS[addl[ss]][1].qS[addl[ss+1]][1]+\[CapitalDelta][ss]qS[addl[ss]][2].qS[addl[ss+1]][2]+qS[addl[ss]][3].qS[addl[ss+1]][3])(*+field[[ss]]qS[ss][fisp]*),{ss,1,length}];
QMSpin=QMSpinsFromHam[length,hamQM,times,initspin,fisp];*)


Dynamic[rr]


<<gaussianinitssu2.wl
TWASp[SU2]=TWASU2Spins;


<<gaussianinits.wl
TWASp[gaus]=TWASU4SpinsGau;


(*<<su4cohinits.wl
TWASp[coh]=TWASU4SpinsWW;*)


runs=1;
<<deltainits.wl
TWASp[delta]=TWASU4Spins;


(*runs=1;
bsites={1,3};
<<XXZhameqns.wl
<<deltainits.wl
TWASp[delta13]=TWASU4Spins;
bsites={2,4};
<<XXZhameqns.wl
<<deltainits.wl
TWASp[delta24]=TWASU4Spins;*)


mmu=MaxMemoryUsed[]/10.^6


(*SetDirectory[ParentDirectory[]];*)


(*Save["4site.dat",{mmu,TWASingle,TWABi}];*)
