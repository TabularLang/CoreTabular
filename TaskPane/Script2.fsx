
//http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

//0-Document abstract shape
type Doc = Concat of Doc * Doc
           | Nil
           | Text of string
           | Line
           | Nest of int * Doc



let layout  (d: Doc)  = "" 
let (<|>) a b = Concat(a,b)


//1-Some type 
type Tree = Node of string * Tree list

//2-Mapping from type to abstract document
let rec showTree (Node(s,ts)) = Text s <|> Nest (s.Length, showBracket ts)and showBracket  = function | [] -> Nil | ts -> Text "[" <|> Nest (1,showTrees ts) <|> Text "]"and showTrees    = function | t::[] -> showTree t                            | t::ts -> showTree t <|> Text "," <|> Line <|> showTrees ts//examplelet v = Node("aaa", [                     Node("bbbbb",[                                    Node("ccc",[]);                                    Node("dd",[])                                   ]                         );                     Node("eee",[                                 Node("ffff",[                                              Node("gg",[]);                                              Node("hhh",[]);                                              Node("ii",[])                                             ]                                     )                                ]                          )                     ]               )let s = showTree v//3-putting abstraction in action

//One simple implementation of simple documents is as strings, with <> as string
//concatenation, nil as the empty string, text as the identity function, line as the
//string consisting of a single newline, nest i as the function that adds i spaces
//after each newline (to increase indentation), and layout as the identity function.
//This simple implmentation is not especially eﬃcient, since nesting examines every
//character of the nested document, nor does it generalize easily. We will consider a
//more algebraic implementation shortly

