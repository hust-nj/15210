functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  datatype carry = GEN | PROP | STOP

  fun x ++ y =
    let
      val lx = length x
      val ly = length y
      fun appone (x,y) = append(x,singleton y)
      val (u,v) = if lx > ly then (appone(x,ZERO),appone(tabulate (fn i => if i < ly then nth y i else ZERO) lx,ZERO))
            else (appone(y,ZERO),appone(tabulate (fn i => if i < lx then nth x i else ZERO) ly,ZERO))  (*长度变为相同*)
      fun judge(x,y) = case (x,y) of
        (ZERO,ZERO)=>STOP
        |(ZERO,ONE)=>PROP
        |(ONE,ZERO)=>PROP
        |(ONE,ONE)=>GEN
      val preres = map2 judge u v(*供查找序列*)
      fun copy (x,PROP) = x
        |copy (_,y) = y
      val (a,last) = scan copy PROP preres
      fun interact (x,y) = case (x,y) of
        (PROP,PROP)=>ONE | (PROP,_)=>ZERO
        |(GEN,PROP)=>ZERO | (GEN,_)=>ONE
        |(STOP,PROP)=>ONE | (STOP,_)=>ZERO
      val result = map2 interact a preres
      fun deal s =(*处理加法或减法运算或分治时拆分产生的前导零*) 
        let val idxs' = enum s
          val idxs = filter (fn (_,x)=>x = ONE) idxs'
        in if length idxs = 0 then empty()
        else take (s,(#1(nth idxs (length idxs - 1))+1))
        end
    in deal result
    end
      

    
  val add = op++
end
