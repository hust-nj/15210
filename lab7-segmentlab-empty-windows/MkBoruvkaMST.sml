functor MkBoruvkaMST (structure Seq : SEQUENCE
                      structure Rand : RANDOM210
                      sharing Seq = Rand.Seq) : MST =
struct
  structure Seq = Rand.Seq
  open Seq

  type vertex = int
  type weight = int
  type edge = vertex * vertex * weight

  (* Remove this exception when you're done! *)
  exception NotYetImplemented

  fun MST (E : edge seq, n : int) : edge seq =
    let
      fun cmp ((_,_,w1),(_,_,w2)) =
        if w1 > w2 then LESS else if w1 < w2 then GREATER else EQUAL
      val edge = sort cmp E  (*sort edges E from largest to smallest by weight*)
      val initedge = enum (map (fn (v1,v2,_) => (v1,v2)) edge) (*map with lable*)
      val initseed = Rand.fromInt 23333
      fun helper(orderE,seed,T) =
        if length orderE = 0 then flatten (%T)
        else
          let
            val coins = Rand.flip seed n
            val ET = map (fn (l,(v1,v2)) => (v1,SOME(l,(v1,v2)))) orderE
            val minE = inject ET (tabulate (fn v => NONE) n)
            val P = map (fn x => case x of NONE => NONE
                                        | SOME(l,(v1,v2))=> if nth coins v1 = 0 andalso nth coins v2 = 1 then SOME(l,v2) else NONE) minE
            val selectE = filter (fn x => x<>NONE) P
            val T' = map (fn(SOME(l,_))=>l) selectE
            val self = map (fn SOME(l,v) => (v,SOME(l,v))) selectE
            val P' = inject self P
            fun produce (l,(v1,v2)) = case (nth P' v1,nth P' v2) of
               (SOME(_,v3),SOME(_,v4)) => if v3 = v4 then empty() else singleton(l,(v3,v4))(*drop the edges in the same partition*)
              |(NONE,SOME(_,v)) => singleton(l,(v1,v))
              |(SOME(_,v),NONE) => singleton(l,(v,v2))
              |(NONE,NONE) => singleton(l,(v1,v2))
            val orderE' = flatten (map produce orderE)
          in helper(orderE',Rand.next seed,T'::T)
          end
    in map (fn i => nth edge i) (helper(initedge,initseed,nil))
    end


end
