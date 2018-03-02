functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  datatype side = left | right
  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    case showt buildings of
      EMPTY => empty()
       |ELT (l,h,r) => %[(l,h),(r,0)]
       |NODE (lseq,rseq) => 
      let val lresult = map (fn(x,y)=>(x,y,left)) (skyline lseq)
        val rresult = map (fn(x,y)=>(x,y,right)) (skyline rseq)
        val xys = merge (fn((x,_,_),(y,_,_))=>if x>y then GREATER else if x=y then EQUAL else LESS) lresult rresult
        (* 通过copyscan找到前边最接近某个点的另一个集合的点
        copy (x,self) = x |copy (x,y) = y ，当scan/reduce到第i个点时，我们便找到了这样的点*)
        fun copy_fits(self) =  fn(x,(a,b,y)) => if y=self then x else (a,b,y) (*单位元为???没有，但是没影响！！！*)
        val front_points_right = scani (copy_fits right) (0,0,right) xys
        val front_points_left = scani (copy_fits left) (0,0,left) xys
        (*利用这些点去产生新的点*)
        fun pairmax((a,b),(a',b'))=(Int.max(a,a'),Int.max(b,b'))
        fun produce (i,point) = case point of (x,y,right) => pairmax((fn(a,b,_)=>(a,b))(nth front_points_right i),(x,y))
                           |(x,y,left)=>pairmax((fn(a,b,_)=>(a,b))(nth front_points_left i),(x,y))
        val result' = mapIdx produce xys
      in filterIdx (fn (i,(_,y)) => i=0 orelse (#2 (nth result' (i-1)) <> y)) result'
      end

end