#if INTERACTIVE
#I @"Bin\Debug" // wherever the .dll files are located
#r @"Infer.Runtime.dll";
#r @"Infer.Compiler.dll";
#r @"Infer.Fun.dll";
#r @"quickgraph.dll";
#r @"quickgraph.serialization.dll";
#else
module pad
#endif



open QuickGraph;
open QuickGraph.Serialization
open System.Xml.Serialization


type PlayGraph () = inherit BidirectionalGraph<int, TaggedEdge<int,int>>()


let g = PlayGraph ()

g.AddVertex(1)
g.AddVertex(2)
g.AddEdge(TaggedEdge<int,int>(1,2,10))
g.ToArrayAdjacencyGraph()


g.AddVertex(1)
g.ToString()



