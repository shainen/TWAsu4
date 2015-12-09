(* ::Package:: *)

(* ::Section:: *)
(*All*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/TWAsu4"];


<<const.wl


<<modfuncs.wl


<<dynfunc.wl


<<runfuncs.wl


(*<<XXZhameqns.wl*)


<<heiswrfham.wl


(*<<su2cohinits.wl*)


<<su4cohinits.wl


(* ::Subsection:: *)
(*run TWA*)


start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3,3}]],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,wignerWeight[[rr]]singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["12site.dat",{mmu,fullTWA4}];
