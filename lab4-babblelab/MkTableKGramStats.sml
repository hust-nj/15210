functor MkTableKGramStats(structure Util : SEQUENCE_UTIL
                          structure T : TABLE
                            where type Key.t = string Util.Seq.seq
                          sharing T.Seq = Util.Seq) : KGRAM_STATS =
struct
  structure Table = T
  structure Seq = T.Seq
  open Util
  open Seq
  exception NotYet
  type token = string
  type kgram = token seq
			
  (* You must define the abstract kgramstats type *)
  type kgramstats = int * (token hist Table.table)
  
  fun makeStats (corpus : string) (maxK : int) : kgramstats =
    let
      val tok = tokens (not o Char.isAlphaNum) corpus
      fun lengthkgram k = tabulate (fn i => (subseq tok (i,k),nth tok (i+k))) ((length tok) - k)
      val last = flatten (tabulate (fn i => lengthkgram i) (maxK+1))
      val res  = Table.map (fn x => histogram String.compare x) (Table.collect last)
    in (maxK,res)
    end

  fun lookupExts (stats : kgramstats) (kgram : kgram) : (token * int) seq = 
    case Table.find (#2 stats) kgram of 
      NONE => empty()
     |SOME th => th

  fun maxK (stats : kgramstats) : int =
    #1 stats
end
