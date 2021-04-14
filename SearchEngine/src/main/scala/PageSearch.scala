import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.ParIterable
import scala.math.log

object PageSearch {
    //Search Algorithim 1
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        (for (x <- pages) yield countTerms(x.text.split("\\s+").toList, query, 0))
    }

    //Search Algorithim 2
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        (for (x <- pages) yield countTerms(x.text.split("\\s+").toList, query, 0) / x.text.replaceAll("\\s", "").length)
    }

    //Search Algorithim 3
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for (x <- pages) yield (for (y <- x.text.split("\\s+").toList if (contains(query.par, y))) yield (countInstances(x.text.split("\\s+").toList.par, y) / x.text.split("\\s+").toList.size) * Math.log(pages.length / (countPageInstances(pages, y, 0)))).toList.sum
    }

    //Checks if the query contains a certain string in parallel
    def contains(query: ParIterable[String], string: String): Boolean = {
        query.foldLeft(false)((a, b) => if (b == string) true else false)
    }

    //Counts the number of terms that appear in both the list of terms and the query
    @tailrec
    def countTerms(termList: List[String], query: List[String], sum: Int): Int = termList match {
        case Nil => sum
        case h::t if(contains(query.par, h)) => countTerms(t.filter(_ == h), query, sum + countInstances(termList.par, h))
        case h::t => countTerms(t.filter(_ == h), query, sum)
    }

    //Counts the number of times a term appears in a term list in parallel
    def countInstances(termList: ParIterable[String], string: String): Int = {
        termList.foldLeft(0)((a, b) => if (b == string) 1 + a else 0 + a)
    }

    //Counts the number of pages a term appears on
    @tailrec
    def countPageInstances(pages: List[RankedWebPage], string: String, sum: Int): Int = pages match {
        case Nil => sum
        case h::t if (contains(h.text.split("\\s+").toList.par, string)) => countPageInstances(t, string, sum + 1)
        case h::t => countPageInstances(t, string, sum)
    }
}