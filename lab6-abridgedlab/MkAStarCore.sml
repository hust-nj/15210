functor MkAStarCore(structure Table : TABLE
                    structure PQ : PQUEUE
                      where type Key.t = real) : ASTAR =
struct
  structure Set = Table.Set
  structure Seq = Set.Seq

  exception SegmentFault

  type weight = real
  type vertex = Set.key
  type edge = vertex * vertex * weight
  type heuristic = vertex -> real
  type 'a table = 'a Table.table


  (* Define this type yourself *)
  type graph = weight table table
  
  fun makeGraph (E : edge Seq.seq) : graph =
    Table.map Table.fromSeq (Table.collect (Seq.map (fn (u,v,w) => (u,(v,w))) E))
  (*A Simple modification of the Dijkstra algorithm*)
  fun findPath h G (S, T) = 
    let
      fun N(v) =
        case Table.find G v
          of NONE => Table.empty()
            |SOME nbr => nbr
      fun astar D Q = 
        case PQ.deleteMin Q
          of (NONE,_) => NONE
           | (SOME (d,v),Q') =>(*d is the actual distance add h v*)
           if Set.find T v then SOME(v,d - h v) else
           case Table.find D v
            of SOME _ =>astar D Q'
              |NONE =>
                let
                  val insert = Table.insert (fn _ => raise SegmentFault)
                  val D' = insert (v,d - h v) D
                  fun relax (q,(u,w)) = PQ.insert ((d - h v + w + h u),u) q
                  val Q'' = Table.iter relax Q' (N v)
                in astar D' Q''
                end
      val initpq = PQ.fromList (Seq.toList (Seq.map (fn p => (h p, p)) (Set.toSeq S)))
    in astar (Table.empty()) initpq
    end

end