functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun x ** y = 
    if length x = 0 orelse length y = 0 then empty()
    else if length x = 1 andalso nth x 0= ONE then y
    else if length y = 1 andalso nth y 0= ONE then x(*induction base*)
      (* (2^n * a + b) * (2^n * c + d) = (2^2n - 2^n) * ac + 2^n * (a+b)(c+d) - (2^n - 1) * bd *)
    else 
      let
        val lx = length x
        val ly = length y 
        val (u,v) = if lx > ly then (x,tabulate (fn i => if i < ly then nth y i else ZERO) lx)
              else (y,tabulate (fn i => if i < lx then nth x i else ZERO) ly)  (*长度变为相同*)
        val hn = (length u) div 2
        (*divide*)
        fun deal s =(*处理加法或减法运算或分治时拆分产生的前导零*) 
        let val idxs' = enum s
          val idxs = filter (fn (_,x)=>x = ONE) idxs'
        in if length idxs = 0 then empty()
        else take (s,(#1(nth idxs (length idxs - 1))+1))
        end
        val a = deal (drop(u,hn))
        val b = deal (take(u,hn))
        val c = deal (drop(v,hn))
        val d = deal (take(v,hn))
        fun power2 n = tabulate (fn x=>ZERO) n
        (*val ac = a**c
        val bd = b**d
        val s = (a++b)**(c++d)*)
        val (ac,bd,s) = par3(fn()=>a**c,fn()=>b**d,fn()=>(a++b)**(c++d))
      in append (power2 (2*hn),ac) -- append (power2 hn,ac) ++ append(power2 hn,s) -- append(power2 hn,bd) ++ bd
      end
	       
  val mul = op**
end
