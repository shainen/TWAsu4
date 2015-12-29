(* ::Package:: *)

(* ::Section:: *)
(*All*)


(* ::Subsubsection:: *)
(*setup*)


SetDirectory[NotebookDirectory[]]


(*SetDirectory[Directory[]<>"/TWAsu4"];*)


<<const.wl


<<modfuncs.wl


<<dynfunc.wl


<<runfuncs.wl


(*<<XXZhameqns.wl*)


<<heiswrfham.wl


(*<<su2cohinits.wl*)


(*<<su4cohinits.wl*)


<<su4cohinitsGaussian.wl


(* ::Subsection:: *)
(*run TWA*)


(*eachTWA2 = Table[solv = NDSolveValue[Flatten[{eqall2, initsSpCoh[rr]}], Flatten[Table[cS[addl[ss]][sp], {ss, length}, {sp, 3, 3}]], {t, 0, tmax}]; {(Through[solv[#]] & /@ times)\[Transpose], Total[(Through[solv[#]] Through[solv[0]] & /@ times)\[Transpose]]/length}, {rr, runs}];//AbsoluteTiming
fullTWA2 = Total[norm^length metricperrun eachTWA2]/runs;*)


Dynamic[rr]


start=First@NDSolve`ProcessEquations[Flatten[{eqall4,initsSingleSpin[1],initsBiSpin[1]}],Flatten[Table[cS[addl[ss]][sp],{ss,length},{sp,3,3}]],{t,0,tmax}];
fullTWA4=0;
Table[AddTo[fullTWA4,(*wignerWeight[[rr]]*)singleRun[start,Flatten[{initsSingleSpin[rr],initsBiSpin[rr]}]]/runs];,{rr,runs}];//AbsoluteTiming


mmu=MaxMemoryUsed[]/10.^6


(*SetDirectory[ParentDirectory[]];*)


(*Save["12site.dat",{mmu,fullTWA4}];*)
