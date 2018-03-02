functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = (set table) * int * int  (*number of edges and vertices*)
  (*remain the number of edges and vertices for they will be needed later.
    To find the outNeighbors quickly, specify it using the table structure*)
  type asp = vertex Seq.seq table
  (*to track back the path of all shortest path between two vertices via asp type, we remain the parents of the discovered vertices with them in bfs, there is the vertex matching to all of their parents in bfs process spcified as a sequence table*)

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    let
      val edgenum = length E
      val adjs = Table.collect E
      val adjt = Table.map Set.fromSeq adjs
      val rangeset = Set.fromSeq (map #2 E)
      val verticesnum = Set.size(Set.union (domain adjs,rangeset))
    in (adjt,edgenum,verticesnum)
    end

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    #2 G

  fun numVertices (G : graph) : int =
    #3 G

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case find (#1 G) v of
      NONE => Seq.empty()
     |SOME(ot) => Set.toSeq ot


  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    case find (#1 G) v of
      NONE => Table.empty()
    | SOME(_) => 
    let
      fun spwithprt(X,F) =(*shortest path with parents*)
        if size F = 0 then X else
          let
            fun vpp s = Seq.map (fn v => (v,s)) (outNeighbors G s) (*vertex with parents pair*)
             (* Work = O(|NGout of s| + log|V|); Span = O(log|V|)*)
            val frontout = flatten(map vpp (Set.toSeq (domain F))) (*merge together*)
            (*work = O(||F|| + |F|log|V|); Span = O(log|V|)*)
            val vwp = Table.collect frontout (*vertex(key) with parents(value)*)
              (*work = O(||F||log|V|); Span = O(log|V| ^2*)
            val newX = Table.merge (fn (x,_) => x) (X,F)
            val newF = Table.erase (vwp,domain newX)
         in spwithprt(newX,newF)
         end
    in spwithprt(Table.empty(),Table.singleton(v,Seq.empty()))
    end

  (* Task 2.5 *)
fun report (A : asp) (v : vertex) : vertex seq seq =
  let
    fun bottomdfs l (s:vertex list list,v) =(*different from dfs for searching to the bottom every time*)
      case find A v of
      NONE => nil
    | SOME(ng) => if length ng = 0 then (v::l)::s
                 else Seq.iter (bottomdfs (v::l)) s ng
    val pathlist = bottomdfs nil (nil,v)
  in Seq.map Seq.fromList (Seq.fromList pathlist)
  end

end
