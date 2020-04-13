namespace Myriad.Plugins

open System
open FSharp.Compiler.Ast
open Myriad.Core


type Plugin =
    | Fields = 1
    //PotentiallyAddOtherBuiltInAttributes



///Inspired by ppx_deriving - `[@@deriving show, eq]`
type DerivingAttribute(plugins: Plugin []) =
    inherit Attribute()

    member __.Plugins = plugins

module Attributes =
    let mapNameToPlugin str =
        match str with
        | "Plugin.Fields" -> Some Plugin.Fields
        | _ -> None

    let getPluginsOnTypeDefinition (td: SynTypeDefn) =
        let rec getIdent (se: SynExpr) =
            match se with
            //Single plugin
            | SynExpr.LongIdent(_,ld,_,_) ->
                ld.Lid
                |> List.map (fun n -> n.idText)
                |> String.concat "."
                |> mapNameToPlugin
                |> Option.map List.singleton
                |> Option.defaultValue []
            //Multiple plugins
            | SynExpr.Sequential(_,true,e1,e2,_) ->
                [
                    yield! getIdent e1
                    yield! getIdent e2
                ]
            | _ -> []


        td
        |> Ast.getAttribute<DerivingAttribute>
        |> Option.bind (fun n ->
            match n.ArgExpr with
            | SynExpr.Paren(SynExpr.ArrayOrListOfSeqExpr(true,SynExpr.CompExpr(true,_,expr ,_),_), _,_,_) ->
                Some (getIdent expr)
            | _ ->
                None)

    let checkIfPluginUsed (plugin: Plugin) (td: SynTypeDefn) =
        match getPluginsOnTypeDefinition td with
        | None -> false
        | Some allPlugins ->
            allPlugins |> List.exists ((=) plugin)
