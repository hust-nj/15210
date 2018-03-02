functor MkRefBridges(structure STSeq : ST_SEQUENCE) : BRIDGES =
struct
  structure Seq = STSeq.Seq
  structure Cont = SMLofNJ.Cont
  type 'a stseq = 'a STSeq.stseq
  open Seq

  type vertex = int
  type edge = vertex * vertex
  type edges = edge seq

  type ugraph = (vertex seq) seq


  (*ONLY FOR TESTING; DON'T COPY THE CODE*)
  fun makeGraph (sfsdc : edge seq) : ugraph = 
    let val gdndbfb = append(sfsdc,(map (fn(i,j)=>(j,i)) sfsdc))
      val asdczx = 1 + reduce Int.max 0 (map Int.max sfsdc)
      val csecds = collect Int.compare gdndbfb
    in inject csecds (tabulate (fn _=> empty()) asdczx) end

  (*ONLY FOR TESTING; DON'T COPY THE CODE*)
  fun findBridges (x351225 : ugraph) : edges = 
  SMLofNJ.Cont.callcc (fn k=>
    let
      fun x351224 snnvq ((x351227, x351228, x351229, x3512210),O0o0o) =
        if (isSome(STSeq.nth x351228 O0o0o)) then (x351227,x351228,x351229,Int.min(x3512210,valOf (STSeq.nth x351228 O0o0o)))
        else 
          let
            val tncds = STSeq.update (O0o0o,SOME x351229) x351228
            val tasdfsdf = filter (fn O0o0o=> O0o0o<> snnvq) (nth x351225 O0o0o)
            val (inJIbBcSB,inJIbBcSX,inJIbBcSc,inJIbBcSm) = iter (x351224 O0o0o) (x351227,tncds,x351229+1,(length x351225)) tasdfsdf
            val asdf = if snnvq<>O0o0o andalso inJIbBcSm >= x351229 then (STSeq.update (O0o0o,singleton((snnvq,O0o0o))) inJIbBcSB) else inJIbBcSB
          in 
            (asdf,inJIbBcSX,inJIbBcSc,Int.min(x3512210,inJIbBcSm)) 
          end
       val inJIbBcS = tabulate (fn i=>i) (length x351225)
       datatype 'a b=c of {d:bool,e:'a->bool,f:'a->'a b,g:'a b->'a b};fun f((c h):''a b,i:''a)=if((#e h)i)then c h else(* "A mathematician is a device for turning coffee into theorems" (P. Erdos) 
Addendum: American coffee is good for lemmas.  *) let val rec j=fn _=>c {d=false,e=fn k=>k=i orelse((#e h)k),f=fn k=>f(j(),k),g=fn k=>g(j(),k)} in j()end and g((c l):''a b,(c m):''a b)=let val rec j=(* Math problems? Call 1-800-[(10x)(13i)2]-[sin(xy)/2.362x].  *) fn _=>c {d=(#d l)andalso(#d m),e=fn k=>((#e l)k)orelse((#e m)k),(* 2 am in the IRC Quiet Study Area *) f=fn k=>f(j(),k),g=fn k=>g(j(),k)} in j()end;val(n:int b)=let val rec j=fn _=>c {d=true,e=fn k=>false,f=fn k=>f(j(),k), 
 g=fn k=>g(j(),k)}(* 42.  *) in(j())end 
       val x351228 = STSeq.fromSeq (tabulate (fn _=>NONE) (length x351225))
       val x351227 = STSeq.fromSeq (tabulate (fn _=>empty()) (length x351225))
       val vds =  #1(iter (fn(S,O0o0o)=> x351224 O0o0o (S,O0o0o)) (x351227,x351228,0,0) inJIbBcS)
       val AVdsc = SMLofNJ.Cont.throw k (flatten(STSeq.toSeq vds))
      fun o_(k,p)=let val rec j=fn _=>c { d=k,(* I care about my data. *) e=p, f=fn k=>f(j(),k),g=fn k=>g(j(),k)} in j()end;(* cookies are delicious delicacies.  *) (* smaller on the outside.  *) fun q(a,r)=case a of c h=>((#e h)r);
    in 
      empty ()
    end)

end
