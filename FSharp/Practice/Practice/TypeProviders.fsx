
#I __SOURCE_DIRECTORY__
#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

open FSharp.Data

let worldBankCountriesXmlPage1 = Http.RequestString "http://api.worldbank.org/country"

type CountriesXml = XmlProvider<"http://api.worldbank.org/country">

let sampleCountries = CountriesXml.GetSample()

sampleCountries.Countries.Length
sampleCountries.Countries.[0].Name

let worldBankCountriesJsonPage1 = Http.RequestString "http://api.worldbank.org/country?format=json"

type CountriesJson = JsonProvider<"http://api.worldbank.org/country?format=json">

let sampleCountriesFromJson = CountriesJson.GetSample()

sampleCountriesFromJson.Array.Length
sampleCountriesFromJson.Array.[0].Name

let loadPageFromXml n =
    CountriesXml.Load(sprintf "http://api.worldbank.org/country?page=%d" n)

let countries = 
    let page1 = loadPageFromXml 1
    [ for i in 1 .. page1.Pages do
        let page = loadPageFromXml i
        yield! page.Countries ]

countries.Length
[for c in countries -> c.Name ]

let loadPageFromJson n =
    CountriesJson.Load(sprintf "http://api.worldbank.org/country?format=json&page=%d" n)

let countriesFromJson =
    let page1 = loadPageFromJson 1
    [ for i in 1 .. page1.Array.Length do
        let page = loadPageFromJson i
        yield! page.Array ]

countriesFromJson.Length
[for c in countriesFromJson -> c.Name ]
