functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
	structure P = P
	open P
	open Seq

	fun parenDist (parens : paren seq) : int option =
		let

	fun parenMatch a=(*判断是否匹配*)
	let 
			fun count (s,x) = case (s,x) of 
			(NONE,_)=>NONE
			|(SOME n,CPAREN)=>if (n=0) then NONE else SOME(n-1)
			|(SOME n,OPAREN)=>SOME(n+1)
	in
		iter count (SOME 0) a=SOME 0
	end

	fun count_match (n:int ,s:paren seq)=(*n表示第n个括号,且nth为左括号*)
		let fun helper(add:int,flag)=
				if flag=0 then add-2
				else case nth s (n+add) of 
					CPAREN =>helper(add+1,flag-1)
					|OPAREN =>helper(add+1,flag+1)
		in helper(1,1)
		end

	fun max_match(s)=
		let fun helper(n,s:paren seq,max)=
			if n<length s then
			case nth s n of 
				CPAREN=>helper(n+1,s,max)
				|OPAREN=>
				let val newmax=(fn(a,b)=>if a>b then a else b )(count_match(n,s),max)
				in helper(n+1,s,newmax)
				end
			else max
		in helper(0,s,0)
		end
	in
			if length parens =0 then NONE
			else
			case parenMatch parens of 
				false => NONE
				|true => SOME (max_match(parens))
	end

			
end
