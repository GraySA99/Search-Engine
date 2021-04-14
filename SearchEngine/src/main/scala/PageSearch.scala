import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.collection.parallel.ParIterable
import scala.math.log

object PageSearch {
    def contains(query: ParIterable[String], string: String): Boolean = {
        query.foldLeft(false)((a, b) => if (b == string) true else false)
    }

    @tailrec
    def countTerms(termList: List[String], query: List[String], sum: Int): Int = termList match {
        case Nil => sum
        case h::t if(contains(query.par, h)) => countTerms(t, query, sum + 1)
        case h::t => countTerms(t, query, sum)
    }

    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        (for (x <- pages) yield countTerms(x.text.split("\\s+").toList, query, 0))
    }

    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        (for (x <- pages) yield countTerms(x.text.split("\\s+").toList, query, 0) / x.text.split("\\s+").toList.size)
    }

    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // TODO: complete implementation
        List[Double](1.0)
    }
}