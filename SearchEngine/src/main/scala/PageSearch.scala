import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.ParIterable
import scala.math.log

object PageSearch {
    //Search Algorithim 1
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        (for (x <- pages) yield countTerms(x.text.split("\\s+").toList.par, query, 0))
    }

    //Search Algorithim 2
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        (for (x <- pages) yield countTerms(x.text.split("\\s+").toList.par, query, 0).toDouble / x.text.split("\\s+").toList.length.toDouble)
    }

    //Search Algorithim 3
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for (x <- pages) yield countSingleTerms(x.text.split("\\s+").toList, query, 0.toDouble, x.text.split("\\s+").toList.size, pages) /*(for (y <- x.text.split("\\s+").toList if (contains(query.par, y))) yield (countInstances(x.text.split("\\s+").toList.par, y).toDouble / x.text.split("\\s+").toList.size.toDouble) * Math.log(pages.length.toDouble / (countPageInstances(pages.par, y, 0)).toDouble)).toList.sum*/
    }

    //Checks if the query contains a certain string in parallel
    def contains(query: ParIterable[String], string: String): Boolean = {
        query.foldLeft(false)((a, b) => if (b.toLowerCase == string.toLowerCase) return !a else a)
    }

    @tailrec
    def countSingleTerms(termList: List[String], query: List[String], weight: Double, initialSize: Int, pages: List[RankedWebPage]): Double = query match {
        case Nil => weight.toDouble
        case h::t if (termList contains(h)) => countSingleTerms(termList.tail.filter(_ != termList.head), t, weight.toDouble + (countInstances(termList.par, h).toDouble / initialSize.toDouble) * (Math.log(pages.length.toDouble / (countPageInstances(pages.par, h, 0)).toDouble)).toDouble, initialSize, pages)
        case h::t if !(termList contains(h)) => countSingleTerms(termList.tail.filter(_ != termList.head), t, weight.toDouble, initialSize, pages)
        //(for (y <- x.text.split("\\s+").toList if (contains(query.par, y))) yield (countInstances(x.text.split("\\s+").toList.par, y).toDouble / x.text.split("\\s+").toList.size.toDouble) * Math.log(pages.length.toDouble / (countPageInstances(pages.par, y, 0)).toDouble)).toList.sum
    }

    //Counts the number of terms that appear in both the list of terms and the query
    /*@tailrec
    def countTerms(termList: List[String], query: List[String], sum: Int): Int = termList match {
        case Nil => sum
        case h::t if(contains(query.par, h)) => countTerms(t.filter(_ != h), query, sum + countInstances(termList.par, h))
        case h::t if(!contains(query.par, h)) => countTerms(t.filter(_ != h), query, sum)
    }*/

    def countTerms(termList: ParIterable[String], query: List[String], sum: Int): Int = {
        termList.foldLeft(0)((a, b) => if(contains(query.par, b)) 1 + a else 0 + a)
    }

    //Counts the number of times a term appears in a term list in parallel
    def countInstances(termList: ParIterable[String], string: String): Int = {
        termList.foldLeft(0)((a, b) => if (b == string) 1 + a else 0 + a)
    }

    //Counts the number of pages a term appears on
    def countPageInstances(pages: ParIterable[RankedWebPage], string: String, sum: Int): Double = {
        pages.foldLeft(0)((a, b) => if (b.text.split("\\s+") contains string) /*(contains(b.text.split("\\s+").toList.par, string))*/ 1 + a else 0 + a)
    }
}