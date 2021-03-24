class WebPage(val id: String, val name: String, val url: String,
              val text: String, val links: List[String]) {

}

// TODO: Define RankedWebPage
class RankedWebPage(id: String, name: String, url: String, text: String, links: List[String]) 
    extends WebPage(id=id, name=name, url=url, text=text, links=links) {}


// TODO: Define SearchedWebPage
class SearchedWebPage(id: String, name: String, url: String, text: String, links: List[String]) 
    extends WebPage(id=id, name=name, url=url, text=text, links=links) {}
