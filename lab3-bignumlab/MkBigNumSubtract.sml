functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y =
  let
  	val ly = length y
  	fun v ONE = ZERO 
  	   |v ZERO = ONE
  	val res' = (map v y)++singleton(ONE)++x
  	val restocount = enum res'
  	val temp = filter (fn(i,x)=>i>ly-1 andalso x = ONE) restocount
 	val res'' = inject (singleton(#1(nth temp 0),ZERO)) res'
 	val res = inject (tabulate (fn i=>(ly+i,ONE)) (#1(nth temp 0)-ly)) res''
  fun deal s =(*处理加法或减法运算或分治时拆分产生的前导零*) 
        let val idxs' = enum s
          val idxs = filter (fn (_,x)=>x = ONE) idxs'
        in if length idxs = 0 then empty()
        else take (s,(#1(nth idxs (length idxs - 1))+1))
        end
 	in deal res
  end
      
  val sub = op--
end
