functor MkBoruvkaSegmenter
  (structure Seq : SEQUENCE
   structure Rand : RANDOM210
   sharing Seq = Rand.Seq)
  : SEGMENTER =
struct
  structure Seq = Rand.Seq
  open Seq

  structure R = Rand
  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight


  fun findSegments (E, n) initial_credit =
    let
      fun cmp ((_,_,w1),(_,_,w2)) = 
        if w1 > w2 then LESS else if w1 < w2 then GREATER else EQUAL
      val initedge = sort cmp E (*sort edges E from largest to smallest*)
      val initseed = Rand.fromInt 233
      val initV = tabulate (fn _ => initial_credit) n
      fun helper(E,V,seed) =
        if length E = 0 then inject (mapIdx (fn (i,_) => (i,i)) V) (tabulate (fn i => ~1)  n)
        else
          let
            val coins = Rand.flip seed n
            val ET = map (fn (v1,v2,w) => (v1,SOME (v1,v2,w))) E
            val minE = inject ET (tabulate (fn v => NONE) n)
            val P = map (fn x => case x of NONE => NONE
                                          |SOME(v1,v2,w) => if nth coins v1 = 0 andalso nth coins v2 = 1 then SOME (v1,v2,w) else NONE) minE
            val P' = filter (fn x => x<> NONE) P
            fun cmp(a,b) = if a>b then GREATER else if a<b then LESS else EQUAL
            val revP = collect cmp (map (fn SOME(v1,v2,w) => (v2,(v1,w,nth V v1))) P')
            fun newV (v,s) = reduce (fn ((_,w1,crdt1),(_,w2,crdt2)) => (v, w1 + w2,Int.min(crdt1,crdt2))) (v,0,nth V v) s 
            val head = map (fn (v,w,crdt) => (v, crdt - w)) (map newV revP)
            val V' = inject head V  (*finish producing new vertices*)
            val P'' = inject (map (fn (v,_) => (v,SOME(v,v,0))) head) P(*add v to self*)
            fun produce (v1,v2,w) = case (nth P'' v1,nth P'' v2) of 
                 (SOME(_,v3,_),SOME(_,v4,_)) => if v3 = v4 then empty() else singleton(v3,v4,w)
                |(NONE,SOME(_,v,_)) => singleton(v1,v,w)
                |(SOME(_,v,_),NONE) => singleton(v,v2,w)
                |(NONE,NONE) => singleton(v1,v2,w)
            val E' = flatten (map produce E)  (*finish produce edges*)
            val selectE = map (fn SOME(x,y,_) => (x,y)) P'
            fun dealedge (v1,v2,w) = if ((nth V v1) + (nth V v2) - w) >=0 then true else false
            val sv = helper(filter dealedge E',V',Rand.next seed)
            val upd = map (fn (v1,v2) => (v1, nth sv v2)) selectE
          in inject upd sv
          end 
    in helper(initedge,initV,initseed)
    end

end
