import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
        (pages.keys map { _ -> 1.0 }).toMap
    }

    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
        pages.keys map (p -> (for (pages <- pages.filter{ _._1 != p }.valuesIterator if pages.links.contains(p)) yield 1).foldLeft(0){_+_})
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation

        def walk(page: WebPage, steps: Int): List[String] = steps match {      

            case x if x <= 0 => List[String]()
            case _ => List[String](page.id) ++ walk((page.links.length == 0 || Random.nextFloat <= 0.15) ? pages.valuesIterator[Random.nextInt(pages.valuesIterator.length)] : page.links[Random.nextInt(pages.links.length)], steps-1)
        } 

        val x: List[String] = (for (_ <- 0 until 10000) yield walk(pages.valuesIterator[Random.nextInt(pages.valuesIterator.length)])).foldLeft(List[String]()){_++_}
        (x map {t => t -> x.count { _ == t } }).toMap
    }
}