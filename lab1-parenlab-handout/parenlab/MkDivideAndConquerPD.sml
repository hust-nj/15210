functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq

  fun parenDist (parens : paren seq) : int option = if length parens = 0 then NONE else
  	let
  		val (max,lnot_match_num,llocal_num,rnot_match_num,rlocal_num)=
	    let
	      	fun helper(C) = 
	      		 case showt C of
	      		 EMPTY =>(0,0,0,0,0)
	      		|ELT n => if n=OPAREN then (0,1,0,0,0)
	      						  else  (0,0,0,1,0)
	      		|NODE(A,B) =>
		      		let
		      			val (a_max,a_lnot_match_num,a_llocal_num,a_rnot_match_num,a_rlocal_num)=helper(A)
		      			val (b_max,b_lnot_match_num,b_llocal_num,b_rnot_match_num,b_rlocal_num)=helper(B)
		      		in  if a_lnot_match_num = b_rnot_match_num then (Int.max((a_llocal_num+b_rlocal_num),Int.max(a_max,b_max)),b_lnot_match_num,b_llocal_num,a_rnot_match_num,a_rlocal_num)
		      		    else if a_lnot_match_num < b_rnot_match_num then (Int.max(a_max,b_max),b_lnot_match_num,b_llocal_num,b_rnot_match_num - a_lnot_match_num + a_rnot_match_num,b_rlocal_num+length A)
		      		    else (Int.max(a_max,b_max),a_lnot_match_num - b_rnot_match_num + b_lnot_match_num,a_llocal_num +length B,a_rnot_match_num,a_rlocal_num)
		      		end
		in helper(parens)
    	end
    in if lnot_match_num = 0 andalso rnot_match_num = 0 then SOME(max)
       else NONE
    end
end

  (*保留哪些数据项？——
  1.当前匹配的最大值max
  2.未在当前串内匹配的最左的左括号在该串未匹配的左括号数目-->lnot_match_num   从右往左计数
  3.                                 中的位置-->llocal_num    从右往左计数
  4.未在当前串内匹配的最右的右括号在该串未匹配的右括号中的第几个-->rnot_match_num   从左往右计数
  5.                                 中的位置-->rlocal_num    从左往右计数
  递推？——    *)
