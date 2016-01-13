module MetaTypes

open Types

type Identifier         = string

type MetaDataValue      = | AllowBackward
                          | Immediate
                          | ForAllSame 
                          | Anticipate
                          | FromTask | FromBelief
 
//type MetaDataTag        = | Truth
//                          | Desire
//                          | Derive
//                          | Eternalize
//                          | Punctuation 
//                          | Order
//                          | Event
//                          | SequenceIntervals

type MetaDataTag        = string

//type MetaTruthFunc      = | BinaryTruth | UnaryTruth
//
//type MetaDataFunc       = | MetaDataTruth | MetaDataValue

type MetaDataFunc       = string

type MetaPredicate      = Identifier * string list

type BinaryTruthFunc    = Truth * Truth -> Truth

type UnaryTruthFunc     = Truth -> Truth


type MetaData           = MetaDataTag * MetaDataFunc
                        
type MetaConclusion     = { Conclusion : Term
                            MetaData : MetaData list}
                        
type MetaRule           = { Premises : string
                            Predicates : string 
                            Conclusion : string * string list}