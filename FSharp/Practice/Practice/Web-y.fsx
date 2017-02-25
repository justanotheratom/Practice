
//------------------------------------------------------------------------------

#I __SOURCE_DIRECTORY__
#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"
#r @"..\packages\Suave.2.0.4\lib\net40\Suave.dll"

open FSharp.Data

type Species = HtmlProvider<"https://en.wikipedia.org/wiki/The_world's_100_most_threatened_species">

let species =
    [ for x in Species.GetSample().Tables.``Species list``.Rows ->
        x.Type, x.``Common name`` ]

let speciesSorted =
    species
    |> List.countBy fst
    |> List.sortByDescending snd

open Suave
open Suave.Web
open Suave.Successful

let html =
    [ yield "<html><body><ul>"
      for (category, count) in speciesSorted do
        yield sprintf "<li>Category <b>%s</b>: <b>%d</b></li>" category count
      yield "</ul></body></html>" ]
    |> String.concat "\n"

let angularHeader = """<head>
    <link rel="stylesheet" href="http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css">
    <script src="http://ajax.googleapis.com/ajax/libs/angularjs/1.2.26/angular.min.js"></script>
    </head>"""

let fancyHtml =
    [ yield """<html>"""
      yield angularHeader
      yield """ <body>"""
      yield """  <table class="table table-stripped">"""
      yield """   <thead><tr><th>Category</th><th>Count</th></tr></thead>"""
      yield """    <tbody"""
      for (category,count) in speciesSorted do
        yield sprintf "<tr><td>%s</td><td>%d</td></tr>" category count
      yield """    </tbody>"""
      yield """  </table>"""
      yield """ </body>"""
      yield """</html>""" ]
    |> String.concat "\n"

startWebServer defaultConfig (OK fancyHtml)

//------------------------------------------------------------------------------

open System.IO
open System.Net

let http (url: string) =
    let req = WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

http "http://news.bbc.co.uk"

//------------------------------------------------------------------------------
