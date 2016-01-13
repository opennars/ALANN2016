// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Program
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

// Create an interactive checker instance 
let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parseAndCheckSingleFile (input) = 
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions = 
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously

    checker.ParseAndCheckProject(projOptions) 
    |> Async.RunSynchronously

//let input2 = 
//    """
//#r @"\\diskstation\homes\Tony\Development\ALANN - Working copy - backup\ALANN_Engine\bin\Debug\ALANN_ENGINE.dll"
//
//module Rules
//let f =
//    //        ((A_1..n) --> M), A_i |- (A_i --> (/,M, A_1..A_i.substitute(_)..A_n )), (Truth:Identity, Desire:Identity)
//    (fun (t : Task, b : Task) (tv1 : Truth, tv2 : Truth) -> 
//                match t.S.Key.Term, b.S.Key.Term with
//                | Inh(Prod(lst), m), Inh(a1, _) when m <> a1 && lst |> List.contains a1
//                    -> [(Inh(a1, ExtImg(m::(sub_ a1 lst))), Some(identity(tv1, tv2)), Some(identity(tv1, tv2)))]
//                | _ -> []
//            ) 
//    """

let input2 = 
      """
module MyLibrary 

open System

let f = 
    function
    | 1 -> "One"
    | _ -> "NotOne"


let foo(x, y) = 
    // comment
    let msg = String.Concat("Hello", " ", "world")
    if msg.Length > 10 then 
        10 
    else 
        20

type MyClass() = 
    member x.MyMethod() = 1
      """    

let checkProjectResults = 
    parseAndCheckSingleFile(input2)

match checkProjectResults.Errors with
| [||] -> ()// should be empty
| err -> failwith (sprintf "unexpected error in source: %A" err)

let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]

let rec printDecl prefix d = 
    match d with 
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> 
        printfn "%sEntity %s was declared and contains %d sub-declarations" prefix e.CompiledName subDecls.Length
        for subDecl in subDecls do 
            printDecl (prefix+"    ") subDecl
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> 
        printfn "%sMember or value %s was declared" prefix  v.CompiledName
    | FSharpImplementationFileDeclaration.InitAction(e) -> 
        printfn "%sA top-level expression was declared" prefix 


for d in checkedFile.Declarations do 
   printDecl "" d

// Entity MyLibrary was declared and contains 4 sub-declarations
//     Member or value foo was declared
//     Entity MyClass was declared and contains 0 sub-declarations
//     Member or value .ctor was declared
//     Member or value MyMethod was declared

let myLibraryEntity, myLibraryDecls =    
   match checkedFile.Declarations.[0] with 
   | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> (e, subDecls)
   | _ -> failwith "unexpected"

let (fooSymbol, fooArgs, fooExpression) =
    match myLibraryDecls.[0] with
    |  FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> (v, vs, e)
    | _ -> failwith "unexpected"

//fooExpression.Type  // shows that the return type of the body expression is 'int'
//fooExpression.Range  // shows the declaration range of the expression implementing 'foo'


let rec visitExpr f (e:FSharpExpr) = 
    f e
    match e with 
    | BasicPatterns.AddressOf(lvalueExpr) -> 
        visitExpr f lvalueExpr
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
        visitExpr f lvalueExpr; visitExpr f rvalueExpr
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        visitExpr f funcExpr; visitExprs f argExprs
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        visitObjArg f objExprOpt; visitExprs f argExprs
    | BasicPatterns.Coerce(targetType, inpExpr) -> 
        visitExpr f inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) -> 
        visitExpr f startExpr; visitExpr f limitExpr; visitExpr f consumeExpr
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
        visitObjArg f objExprOpt
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
        visitObjArg f objExprOpt
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
        visitExpr f guardExpr; visitExpr f thenExpr; visitExpr f elseExpr
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) -> 
        visitExpr f bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) -> 
        visitExpr f bindingExpr; visitExpr f bodyExpr
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
        List.iter (snd >> visitExpr f) recursiveBindings; visitExpr f bodyExpr
    | BasicPatterns.NewArray(arrayType, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
        visitExpr f delegateBodyExpr
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.NewRecord(recordType, argExprs) ->  
        visitExprs f argExprs
    | BasicPatterns.NewTuple(tupleType, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.Quote(quotedExpr) -> 
        visitExpr f quotedExpr
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
        visitObjArg f objExprOpt
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
        visitObjArg f objExprOpt; visitExpr f argExpr
    | BasicPatterns.Sequential(firstExpr, secondExpr) -> 
        visitExpr f firstExpr; visitExpr f secondExpr
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) -> 
        visitExpr f bodyExpr; visitExpr f finalizeExpr
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) -> 
        visitExpr f bodyExpr; visitExpr f catchExpr
    | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
        visitExpr f tupleExpr
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
        visitExpr f decisionExpr; List.iter (snd >> visitExpr f) decisionTargets
    | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
        visitExprs f decisionTargetExprs
    | BasicPatterns.TypeLambda(genericParam, bodyExpr) -> 
        visitExpr f bodyExpr
    | BasicPatterns.TypeTest(ty, inpExpr) -> 
        visitExpr f inpExpr
    | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
        visitExpr f unionExpr; visitExpr f valueExpr
    | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
        visitExpr f unionExpr
    | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
        visitExpr f unionExpr
    | BasicPatterns.UnionCaseTag(unionExpr, unionType) -> 
        visitExpr f unionExpr
    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
        visitExpr f baseCallExpr
        List.iter (visitObjMember f) overrides
        List.iter (snd >> List.iter (visitObjMember f)) interfaceImplementations
    | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argExprs) -> 
        visitExprs f argExprs
    | BasicPatterns.ValueSet(valToSet, valueExpr) -> 
        visitExpr f valueExpr
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) -> 
        visitExpr f guardExpr; visitExpr f bodyExpr
    | BasicPatterns.BaseValue baseType -> ()
    | BasicPatterns.DefaultValue defaultType -> ()
    | BasicPatterns.ThisValue thisType -> ()
    | BasicPatterns.Const(constValueObj, constType) -> ()
    | BasicPatterns.Value(valueToGet) -> ()
    | _ -> failwith (sprintf "unrecognized %+A" e)

and visitExprs f exprs = 
    List.iter (visitExpr f) exprs

and visitObjArg f objOpt = 
    Option.iter (visitExpr f) objOpt

and visitObjMember f memb = 
    visitExpr f memb.Body

fooExpression |> visitExpr (fun e -> printfn "Visiting %A" e)

// Prints:
//
// Visiting Let...
// Visiting Call...
// Visiting Const ("Hello", ...)
// Visiting Const (" ", ...)
// Visiting Const ("world", ...)
// Visiting IfThenElse...
// Visiting Call...
// Visiting Call...
// Visiting Value ...
// Visiting Const ...
// Visiting Const ...
// Visiting Const ...
Console.ReadLine() |> ignore