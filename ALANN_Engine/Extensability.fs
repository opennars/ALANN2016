module Extensability

//open System.ComponentModel.Composition
//open System.ComponentModel.Composition.Hosting
//
//
//// Expose an interface
//type IInferenceLevel =
//    abstract Inference : InferenceElement -> Task list        
//
//type NALLevelCollection() = 
//   [<ImportMany(typeof<IInferenceLevel>)>] 
//   let levels : seq<IInferenceLevel> = null
//
//   member __.Inference(ie) =
//       (levels |> Seq.map (fun level -> level.Inference(ie))) |> Seq.concat
//
//type InferenceEngine() =                     
//    // Set up MEF
//    let catalog = new AggregateCatalog()
//    let directoryCatalog = new DirectoryCatalog(@"c:\Extensions","*.dll")
//    let container = new CompositionContainer(catalog)
//    let NALLevels = NALLevelCollection()  
//    do
//        catalog.Catalogs.Add(directoryCatalog)                  
//        container.ComposeParts(NALLevels)
//
//    member __.Inference(ie : InferenceElement) =
//        NALLevels.Inference(ie)
//
//  [<Export(typeof<IInferenceLevel>)>]
//    type NALLevel1() =