import annotation.tailrec
import collection.mutable.HashSet
import io.Source

/**
 *
 *
 * @author John Kenny
 * @since 03/05/2012
 */
object Words {

  val bruteForce = true

  case class Neighbour(word: String, cost: Int, newWord: String) extends Ordered[Neighbour] {
    def compare(that: Neighbour) = cost.compareTo(that.cost)
  }

  case class Word(word: String, neighbours: Iterable[Neighbour]) extends Ordered[Word] {
    val averageCost: Float = neighbours.foldLeft(0)((res, n) => res + n.cost).toFloat / neighbours.size

    //    def compare(that: Word) = that.averageCost.compareTo(averageCost)
    def compare(that: Word) = neighbours.head.cost.compareTo(that.neighbours.head.cost)
  }

  def main(args: Array[String]) {

    val source = Source.fromFile("words.txt")

    val words = source.getLines().toIterable

    val shortest = if (bruteForce) smartBruteForce(words) else solve(words)

    println(shortest)
    println("Length = " + shortest.length)
  }

  private def bruteForce(words: Iterable[String]): String = {
    var shortest: String = null

    val solutions = words.toList.permutations.toSeq.par.map(solve(_)).seq

    solutions.foreach {
      solution =>
        if (shortest == null || solution.length < shortest.length) {
          shortest = solution
        }
    }

    shortest
  }

  private def smartBruteForce(words: Iterable[String]): String = {
    var shortest: String = null

    val startTime = System.currentTimeMillis()

    words.toList.permutations.find {
      puzzle =>
        val solution = solve(puzzle)
        if (solution.length < 50 || (System.currentTimeMillis() - startTime) > 300000) {
          shortest = solution
          true
        } else {
          false
        }
    }

    shortest
  }

  private def buildNeighbours(word: String, words: Iterable[String]): Iterable[Neighbour] = {
    (words.filter(_ != word).map {
      neighbour =>
        val merge = mergeFast(word, neighbour)
        Neighbour(neighbour, merge._2, merge._1)
    }).toSeq.sorted
  }

  private def buildWords(words: Iterable[String]): Iterable[Word] = {
    val richWords = words.map(word => Word(word, buildNeighbours(word, words)))
    if (!bruteForce) {
      richWords.toSeq.sorted
    } else {
      richWords
    }
  }

  @tailrec
  private def solve(words: Iterable[String]): String = {
    if (words.size == 1) return words.head

    val richWords: Iterable[Word] = buildWords(words)
    val alreadyChosen = HashSet.empty[String]

    solve(richWords.foldLeft(Iterable.empty[String]) {
      (res, richWord) =>
        if (alreadyChosen.contains(richWord.word)) {
          res
        } else {
          val optNeighbour = richWord.neighbours.find(n => !alreadyChosen.contains(n.word))
          if (optNeighbour.isDefined) {
            alreadyChosen.add(richWord.word)
            alreadyChosen.add(optNeighbour.get.word)

            res ++ Iterable(optNeighbour.get.newWord)
          } else {
            res ++ Iterable(richWord.word)
          }
        }
    })
  }

  def mergeFast(a: String, b: String): (String, Int) = {
    val aArr = a.toCharArray
    val bArr = b.toCharArray
    val resultBuffer = new Array[Char](a.length + b.length)
    var i = 1
    val limit = aArr.length
    while (i < limit) {
      var matches = true
      var j = 0
      val limit2 = math.min(aArr.length - i, bArr.length)
      while (matches && j < limit2 && matches) {
        if (aArr(i + j) != bArr(j)) {
          matches = false
        }
        j += 1
      }
      if (matches) {
        if (i + bArr.length < aArr.length) {
          return (a, 0)
        } else {
          System.arraycopy(aArr, 0, resultBuffer, 0, i)
          System.arraycopy(bArr, 0, resultBuffer, i, bArr.length)

          val newString = new String(resultBuffer, 0, i + bArr.length)
          return (newString, newString.length - a.length)
        }
      }
      i += 1
    }
    // because we removed degeneracies in advance, we don't need to check
    // whether b contains a here.  If we didn't remove the degeneracies,
    // we would need a check here like:
    //    if (b contains a) {
    //      return b
    //    }
    // The algorithm above catches the a contains b case already
    System.arraycopy(aArr, 0, resultBuffer, 0, aArr.length)
    System.arraycopy(bArr, 0, resultBuffer, aArr.length, bArr.length)

    val newString = new String(resultBuffer, 0, resultBuffer.length)

    (newString, newString.length - a.length)
  }
}

/*

Neighbour (word, cost)
Node (word, neighbours sorted by cost, average cost)


For a list of words, transform it into a list of Words sorted by smallest Neighbour cost
For a list of Words, transform it into a smaller list of words by:
  If the word hasn't already been chosen:
    Join it to its smallest neighbour that hasn't already been chosen
    Add the word and its neighbour to the list of chosen words

*/
