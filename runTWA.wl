(* ::Package:: *)

(* ::Section:: *)
(*All*)


(*SetDirectory[NotebookDirectory[]]*)


SetDirectory[Directory[]<>"/TWAsu4"];


<<const.wl


<<modfuncs.wl


<<dynfunc.wl


(*<<XXZhameqns.wl*)


<<heiswrfham.wl


(*<<su2cohinits.wl*)


<<su4cohinits.wl


(* ::Subsection:: *)
(*run TWA*)


(*eachTWA2 = Table[solv = NDSolveValue[Flatten[{eqall2, initsSpCoh[rr]}], Flatten[Table[cS[addl[ss]][sp], {ss, length}, {sp, 3, 3}]], {t, 0, tmax}]; {(Through[solv[#]] & /@ times)\[Transpose], Total[(Through[solv[#]] Through[solv[0]] & /@ times)\[Transpose]]/length}, {rr, runs}];
fullTWA2 = Total[norm^length metricperrun eachTWA2]/runs;*)


eachTWA4=Table[solv=NDSolveValue[Flatten[{eqall4,initsSingleSpin[rr],initsBiSpin[rr]}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3,3}]],{t,0,tmax}];{(Through[solv[#]]&/@times)\[Transpose],Total[(Through[solv[#]]Through[solv[0]]&/@times)\[Transpose]]/length},{rr,runs}];
fullTWA4=Total[wignerWeight eachTWA4]/runs;


mmu=MaxMemoryUsed[]/10.^6;


SetDirectory[ParentDirectory[]];


Save["12site.dat",{mmu,fullTWA4}];
