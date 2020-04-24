import forcomp.Anagrams.{Occurrences, Sentence, Word, combinations, dictionary, dictionaryByOccurrences, sentenceOccurrences, subtract, wordOccurrences}


def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  def sentenceBuilder(occurrences: Occurrences): List[Sentence] = {
    if (occurrences.isEmpty) List(Nil)
    else for {
      suboccs <- combinations(occurrences)
      word <- dictionaryByOccurrences.getOrElse(suboccs, Nil)
      sentence <- sentenceBuilder(subtract(occurrences, wordOccurrences(word)))
    } yield word :: sentence
  }

  sentenceBuilder(sentenceOccurrences(sentence))
}


sentenceAnagrams(List("you", "olive"))
var s = sentenceOccurrences(List("you", "olive"))
var suboccs = combinations(s)

suboccs.foreach(x => println(x))

var words: List[Any] = suboccs.map(s => dictionaryByOccurrences.getOrElse(s, Nil)).toSet.toList

words.foreach(x => println(x))

def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.sorted match {
  case Nil => Nil
  case x :: xs => (x, w.toLowerCase.count(_ == x)) :: wordOccurrences(xs.dropWhile(_ == x).mkString)
}

wordOccurrences("Eve")
