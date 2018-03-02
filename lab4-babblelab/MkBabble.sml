functor MkBabble(structure R : RANDOM210
                 structure KS : KGRAM_STATS
                 structure Util : SEQUENCE_UTIL
                 sharing KS.Seq = Util.Seq
                 sharing KS.Seq = R.Seq) : BABBLE =
struct
  structure Rand = R
  structure Stats = KS
  open Stats.Seq
  exception Range
  exception Size

fun subseq _ (0, 0) = []
    | subseq [] (0, _) = raise Size
    | subseq [] _ = raise Range
    | subseq (x::xs) (i, len) =
      case Int.compare (i, 0)
        of EQUAL => x::subseq xs (0, len-1)
         | LESS => raise Range
         | GREATER => subseq xs (i-1, len)

  fun randomSentence (stats : KS.kgramstats) (n : int) (seed : R.rand) =
    let
      val rrseq = Rand.randomRealSeq seed NONE n
      fun next l len = (*len is the length of l*)
        let 
          fun helper k = (*k is the length of gram*)
            let val temp = Stats.lookupExts stats (fromList(List.rev(subseq l (0, Int.min(len, k)))))
            in if length temp = 0 then helper (k-1)
              else temp
            end
        in helper (Stats.maxK stats)
        end
      fun output L i = (*i represent the length of L*)
        if i = n then L 
        else output ((Util.choose (next L i) (nth rrseq i))::L) (i+1)
      val res = List.rev(output nil 0)
    in (String.concatWith " " res) ^ "."
    end
               
    fun randomDocument (stats : KS.kgramstats) (n : int) (seed : R.rand) =
      let 
        val lenseq = Rand.randomIntSeq seed (SOME(5,11)) n
        val seedseq = map R.fromInt (Rand.randomIntSeq seed NONE n)
        fun combine (length,seed) = randomSentence stats length seed
        val result = map2 combine lenseq seedseq
      in String.concatWith " " (toList result)
      end
      
end
