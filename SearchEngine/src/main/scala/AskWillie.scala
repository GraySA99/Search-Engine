import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Sorting

object AskWillie {
    def main(args: Array[String]) = {
        // println("=============================================================")
        // println("   _____          __      __      __.__.__  .__  .__ ")
        // println("  /  _  \\   _____|  | __ /  \\    /  \\__|  | |  | |__| ____  ")
        // println(" /  /_\\  \\ /  ___/  |/ / \\   \\/\\/   /  |  | |  | |  |/ __ \\")
        // println("/    |    \\___ \\|     <   \\        /|  |  |_|  |_|  \\  ___/ ")
        // println("\\____|__  /____  >__|_ \\   \\__/\\  / |__|____/____/__|\\___  >")
        // println("        \\/     \\/     \\/        \\/                       \\/")
        // println("=============================================================")

        println("==================================================================================")   
        println("   _____    _____________  __.")
        println("  /  _  \\  /   _____/    |/ _|")
        println(" /  /_\\  \\ \\_____  \\|      <  ")
        println("/    |    \\/        \\    |  \\ ")
        println("\\____|__  /_______  /____|__ \\")
        println("        \\/        \\/        \\/")
        println("  _____________________________    _____      _____    _______  ___________")
        println(" /  _____/\\_   _____/\\______   \\  /     \\    /  _  \\   \\      \\ \\_   _____/")
        println("/   \\  ___ |    __)_  |       _/ /  \\ /  \\  /  /_\\  \\  /   |   \\ |    __)_ ")
        println("\\    \\_\\  \\|        \\ |    |   \\/    Y    \\/    |    \\/    |    \\|        \\")
        println(" \\______  /_______  / |____|_  /\\____|__  /\\____|__  /\\____|__  /_______  /")
        println("        \\/        \\/         \\/         \\/         \\/         \\/        \\/ ")
        println("==================================================================================") 

        // Load WebPage.id -> WebPage map to better handle graph
        val pages: Map[String, WebPage] = mapWebPages(loadWebPages)
        //print("not")
        val ranks: Map[String, Double] = PageRank.pagerank(pages)

        //print("here")

        val max = ranks.max._2
        val min = ranks.min._2

        val normalRank = for((key,value) <- ranks) yield (key -> (value-min)/(max-min))

        val rankedPages = for((key, value) <- pages) yield new RankedWebPage(value.id, value.name, value.url, value.text, value.links, normalRank.getOrElse(key, 0))

        print("|-/ ")
        var userInput = readLine() 
        
        while (userInput != ":quit") {

            val words = userInput.split(" ")
            val matches = PageSearch.count(rankedPages.toList, words.toList)
            val normalMatch = for(value <- matches) yield ((value-matches.min)/(matches.max-matches.min))

            val searchPages = for((page, matches) <- rankedPages zip normalMatch) yield new SearchedWebPage(page.id, page.name, page.url, page.text, page.links, page.weight, matches)

            Sorting.quickSort(searchPages.toArray)(pageOrdering)

            val bestPages = for(i <- 0 until 10) yield searchPages.toList(i)

            val bestList: List[String] = for(page <- bestPages.toList) yield (page.name + ": "+ page.url+ "\n")
            for(site <- bestList){
                print(site)
            }


            print("|-/ ")
            userInput = readLine()  
        }

    }

    

    object pageOrdering extends Ordering[SearchedWebPage] {

        def arithmetic(weight: Double, rank: Double) = {(weight + rank)/2}
        def geometric(weight: Double, rank: Double) = {math.sqrt(weight * rank)}
        def harmonic(weight: Double, rank: Double) = {2/((1/weight) + (1/rank))}

        def compare(a:SearchedWebPage, b:SearchedWebPage) = arithmetic(a.weight, a.textMatch) compare arithmetic(b.weight, b.textMatch)
    }

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