open Infrastructure
open Json

let nullExample = "null"
run jValue nullExample |> printResult

let boolExample = "true"
run jValue boolExample |> printResult

let numberExample = "123.4"
run jValue numberExample |> printResult

let stringExample = "\"abc\""
run jValue stringExample |> printResult

let objectExample = """{
    "name": "jeffrey",
    "age": 18,
    "address": { "country": "china", "city": "shenzhen" },
    "cards": [ "id", "social security" ]
}"""
run jValue objectExample |> printResult

let arrayExample = "[ 1, 2, 3 ]"
run jValue arrayExample |> printResult
