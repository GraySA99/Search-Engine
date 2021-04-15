class WebPage(val id: String, val name: String, val url: String,
              val text: String, val links: List[String]) {

}

// TODO: Define RankedWebPage
class RankedWebPage(override val id: String, override val name: String, override val url: String, override val text: String, override val links: List[String], val weight: Double) 
    extends WebPage(id=id, name=name, url=url, text=text, links=links) {}


// TODO: Define SearchedWebPage
class SearchedWebPage(override val id: String, override val name: String, override val url: String, override val text: String, override val links: List[String], override val weight: Double, val textMatch: Double) 
    extends RankedWebPage(id=id, name=name, url=url, text=text, links=links, weight=weight) {}
