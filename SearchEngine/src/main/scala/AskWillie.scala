import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Sorting

object AskWillie {
    def main(args: Array[String]) = {
        println("=============================================================")
        println("   _____          __      __      __.__.__  .__  .__ ")
        println("  /  _  \\   _____|  | __ /  \\    /  \\__|  | |  | |__| ____  ")
        println(" /  /_\\  \\ /  ___/  |/ / \\   \\/\\/   /  |  | |  | |  |/ __ \\")
        println("/    |    \\___ \\|     <   \\        /|  |  |_|  |_|  \\  ___/ ")
        println("\\____|__  /____  >__|_ \\   \\__/\\  / |__|____/____/__|\\___  >")
        println("        \\/     \\/     \\/        \\/                       \\/")
        println("=============================================================")

        // Load WebPage.id -> WebPage map to better handle graph
        val pages: Map[String, WebPage] = mapWebPages(loadWebPages)
        //print("not")
        val ranks: Map[String, Double] = PageRank.pagerank(pages)

        //print("here")

        val max = ranks.max._2
        val min = ranks.min._2

        val normalRank = for((key,value) <- ranks) yield (key -> (value-min)/(max-min))

        val rankedPages = for((key, value) <- pages) yield new RankedWebPage(value.id, value.name, value.url, value.text, value.links, normalRank.getOrElse(key, 0))

        
        var userInput = ""

        while (userInput != ":quit") {
            print("|-/ ")
            userInput = readLine()  
            val words = userInput.split(" ")
            val matches = PageSearch.count(rankedPages.toList, words.toList)
            val normalMatch = for(value <- matches) yield ((value-matches.min)/(matches.max-matches.min))
            print(rankedPages)
            print(words)
            print("results:"+matches)

            val searchPages = for((page, matches) <- rankedPages zip normalMatch) yield new SearchedWebPage(page.id, page.name, page.url, page.text, page.links, page.weight, matches)

            //val pageTuple = Array(for(page <- searchPages) yield (page, Arithmetic(page.weight, page.textmatch)))
            //check _._2 please
            //val pageSort = Sorting.quickSort(pageTuple)(Ordering.by[SearchedWebPage, Double](_._2))
        }

    }

    def Arithmetic(weight: Double, rank: Double) = {(weight + rank)/2}
    def Geometric(weight: Double, rank: Double) = {math.sqrt(weight * rank)}
    def harmonic(weight: Double, rank: Double) = {2/((1/weight) + (1/rank))}

    // Load a List of WebPage objects from the packaged prolandwiki.csv file
    def loadWebPages: List[WebPage] = {
        // create an input stream to the proglangwiki.csv
        val fh = Source.fromInputStream(
            getClass.getClassLoader.getResourceAsStream("proglangwiki.csv"))
        // load all pages from the file line by line
        val pages = (for (line <- fh.getLines) yield {
            val id::name::url::text::links = line.split(",").toList
            new WebPage(id, name, url, text, links)
        }).toList
        fh.close
        pages
    }

    // Convert a List[WebPage] to a Map[String, WebPage]
    def mapWebPages(pages: List[WebPage]): Map[String, WebPage] = {
        (for (page <- pages) yield (page.id, page)).toMap
    }
}