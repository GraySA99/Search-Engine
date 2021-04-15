import scala.util.Random
import scala.collection.parallel.CollectionConverters._

object PageRank {
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
        (pages.keys map { _ -> 1.0 }).toMap
    }

    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation
        (pages.keys map { p => p -> 1.0*((for (pages <- pages.filter{ _._1 != p }.valuesIterator if pages.links.contains(p)) yield 1).foldLeft(0){_+_}) }).toMap
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        // TODO: complete implementation

        def walk(page: WebPage, steps: Int): List[String] = steps match {      

            case x if x <= 0 => List[String]()
            case _ => {
                if (page.links.length == 0 || Random.nextFloat <= 0.15) {
                    val w = pages.values.toList(Random.nextInt(pages.values.size))
                    List[String](page.id) ++ walk(w, steps-1)
                } else {
                    val w = page.links(Random.nextInt(page.links.length))
                    List[String](page.id) ++ walk(pages(w), steps-1)
                }
            } 
        } 

        val x: List[String] = ( (1 to 10000).toList.par.map(_ => walk(pages.values.toList(Random.nextInt(pages.values.size)), 100)) ).foldLeft(List[String]()){_++_}
        // val x: List[String] = (for (_ <- 0 until 10000) yield walk(pages.values.toList(Random.nextInt(pages.values.size)), 100)).foldLeft(List[String]()){_++_}
        (x map {t => t -> (x.count { _ == t })*1.0 }).toMap
    }
}