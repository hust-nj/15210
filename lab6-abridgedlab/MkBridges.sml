functor MkBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  type 'a stseq = 'a STSeq.stseq
  open Seq

  exception NYI

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = vertex seq seq

  fun makeGraph (E : edge seq) : ugraph = 
    let 
      val edgeseq = append(map (fn(u,v) => (v,u)) E, E)
      val res = collect Int.compare edgeseq
    in map (fn (x,y) => y) res
    end

  fun findBridges (G : ugraph) : edges = 
    let 
      val newG = append(G,singleton (tabulate (fn i => i) (length G)))(*in order to run all vertices*)
      val numV = length newG
      val initX = STSeq.fromSeq (tabulate (fn i => false) numV)  (*discoverd vertices*)
      val initminV = STSeq.fromSeq (tabulate (fn i => 0) numV) (*updated min value*)
      fun updatemin p v minV= if STSeq.nth minV p > STSeq.nth minV v then STSeq.update (p,STSeq.nth minV v) minV
                          else minV
      fun bridge p v i l minV= if STSeq.nth minV v > i-1 then (p,v)::l else l(*i-1 is the lable of p*)
      fun dfs p ((X, minV, L, i),v) =
        case STSeq.nth X v of
          true => (X, updatemin p v minV, L, i)  (*revisit v*)
          |false => 
          let
            val X' = STSeq.update (v, true) X
            val minV' = STSeq.update (v,i) minV (*discover v*)
            val (X'',minV'', L',i') = iter (dfs v) (X', minV',L,i+1) (filter (fn x => x<>p) (nth newG v))
            val minV''' = updatemin p v minV''
            val L'' = bridge p v i L' minV'''(*finish v*)
          in (X'', minV''',L'', i')
          end
      val res = (fn(_,_,l,_) =>l)(dfs (numV-1) ((initX,initminV,nil,0),numV-1))
    in filter(fn(p,_)=>p<>numV-1)(fromList res)
    end
end
