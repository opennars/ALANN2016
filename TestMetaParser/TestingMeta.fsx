
#r @"D:\Visual Studio 2015\Projects\ALANN\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"D:\Visual Studio 2015\Projects\ALANN\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"
#r @"D:\Visual Studio 2015\Projects\ALANN\MetaRuleParser\bin\Debug\MetaRuleParser.dll"

open RuleParser
open ParserUtils

parseFileToTasks(@"D:\Visual Studio 2015\Projects\ALANN\MetaRuleParser\MetaRules.txt")