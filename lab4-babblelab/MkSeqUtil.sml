functor MkSeqUtil(structure S : SEQUENCE) : SEQUENCE_UTIL =
struct
  structure Seq = S
  open Seq

  type 'a hist = ('a * int) seq

  exception Range

  fun tokens (cp : char -> bool) (str : string) : string seq =
    let
      val n = String.size str
      val chars = tabulate (fn i => (i, String.sub (str, i))) n
      val idx = map (fn (i,_) => i) (filter (fn (_,c) => cp c) chars)

      (* grab substrings in between delimiters *)
      val subs = map2 (fn (i,i') => String.substring (str, i, i' - i))
                      (append (singleton 0, map (fn i => i + 1) idx))
                      (append (idx, singleton n))
    in filter (fn s => size s > 0) subs
    end

  fun histogram (cmp : 'a ord) (s : 'a seq) : 'a hist =
    map (fn (a, c) => (a, length c))
        (collect cmp (map (fn a => (a, ())) s))

  fun choose (hist : 'a hist) (p : real) : 'a =
   if length hist = 0 orelse p < 0.0 orelse p > 1.0 then raise Range
   else
   let
    val range = scani op+ 0 (map (fn(_,i)=>i) hist);
    val enrange = enum range;
    val value = Real.ceil (p * Real.fromInt(nth range (length range - 1)))
    (*进行二分查找*)
    fun search(x,s) = 
      if length s = 1 then (fn(i,_)=>i)(nth s 0) else
      let val halfl = length s div 2
      in if x <= (fn(_,a)=>a)(nth s (halfl-1)) then search(x,take(s,halfl) )
         else search(x,drop(s,halfl))
      end
    in (fn(a,_)=>a)(nth hist (search(value,enrange)))
    end

end
