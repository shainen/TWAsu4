(* ::Package:: *)

singleRun=Function[{startEq,newInits},
Block[{newstate=First@NDSolve`Reinitialize[start,newInits],sol},
NDSolve`Iterate[newstate,tmax];
sol=NDSolve`ProcessSolutions[newstate][[All,2]];
(Through[sol[#]]&/@times)\[Transpose]
]
];
