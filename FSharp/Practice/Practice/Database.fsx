
#I __SOURCE_DIRECTORY__
#r @"..\packages\FSharp.Data.TypeProviders.5.0.0.2\lib\net40\FSharp.Data.TypeProviders.dll"
#r "System.Data.Linq.dll"

open System
open FSharp.Linq
open FSharp.Data.TypeProviders

// This is not quite working.
//type NorthwindDb =
//    SqlDataConnection<ConnectionString =
//                        @"AttachDBFileName = 'C:\Program Files\Microsoft SQL Server\MSSQL13.SQLEXPRESS\MSSQL\DATA\Northwind.mdf';
//                          Server='SANKETP-SB1\SQLEXPRESS';
//                          User Instance=true;
//                          Integrated Security=SSPI",
//                      Pluralize=true>

type NorthwindDb =
    SqlDataConnection<"Data Source=SANKETP-SB1\SQLEXPRESS;Initial Catalog=Northwind;Integrated Security=SSPI;">

let db = NorthwindDb.GetDataContext()
db.DataContext.Log <- System.Console.Out

let realize query =
    query |> Seq.truncate 10 |> Seq.toList

let customersSortedByCountry =
    query { for c in db.Customers do
            sortBy c.Country
            select (c.Country, c.CompanyName) }
    |> realize

let selectedEmployees =
    query { for e in db.Employees do
            where (e.BirthDate.Value.Year > 1960)
            where (e.LastName.StartsWith "S")
            select (e.FirstName, e.LastName)
            take 5 }
    |> realize

let customersSortedTwoColumns =
    query { for c in db.Customers do
            sortBy c.Country
            thenBy c.Region
            select (c.Country, c.Region, c.CompanyName) }
    |> realize

let totalOrderQuantity =
    query { for o in db.OrderDetails do
            sumBy (int o.Quantity) }

let customersAverageOrders =
    query { for c in db.Customers do
            averageBy (float c.Orders.Count) }

let averagePriceOverProduceRange =
    query { for p in db.Products do
            averageByNullable p.UnitPrice }

let perCustomerOrderStats =
    query {
        for c in db.Customers do
            // LL: Can't do this instead
            //let orderCount = sumByNullable (Nullable (int c.Orders.OrderDetails.Quantity))
            let orderCount =
                query {
                    for o in c.Orders do
                    for od in o.OrderDetails do
                    sumByNullable (Nullable(int od.Quantity))
                }
            let averagePrice =
                query {
                    for o in c.Orders do
                    for od in o.OrderDetails do
                    averageByNullable (Nullable(od.UnitPrice))
                }
            select (c.ContactName, orderCount, averagePrice)
    }
    |> realize

let productsGroupedByNameAndCounted =
    query {
        for p in db.Products do
        groupBy p.CategoryID.CategoryName into group // ??: Does not compile.
        let sum =
            query {
                for p in group do
                sumBy (int p.UnitsInStock.Value)
            }
        select (group.Key, sum)
    }
    |> realize

let innerJoinQuery =
    query {
        for c in db.Categories do
        join p in db.Products on (c.CategoryID =? p.CategoryID)
        select (p.ProductName, c.CategoryName)
    }
    |> realize

let innerJoinAlternative =
    query {
        for p in db.Products do
        select (p.ProductName, p.Category.CategoryName) // ??: Does not compile.
    }
    |> Seq.toList
    |> realize

