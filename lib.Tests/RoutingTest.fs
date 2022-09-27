module lib.Routing.Tests

open NUnit.Framework
open Lib
open Routing

[<Test>]
let ParserTest () =
    let parser = pConst "author" </> pInt </> pStr
    let result = parser "author/123/John"

    Assert.That(result, Is.EqualTo(Some ((("author",123), "John"),"")))
