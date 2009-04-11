#light
// Copyright (c) 2009 All Right Reserved, Robert Pickering
//
// This source is subject to the GLPv2, please see Strangelights.DataTools.gpl-2.0.txt.
// Contact Robert Pickering via: http://strangelights.com/

module Strangelights.DataTools.Gutenberg
open System
open System.IO
open System.IO.Compression
open System.Net
open System.Xml
open System.Xml.XPath
open ICSharpCode.SharpZipLib.Zip

let DefaultIndexUrl =  "http://www.gutenberg.org/feeds/catalog.rdf.zip"

type Book = 
    { Id: string;
      Title: string;
      Author: string }

type GutenbergIndexFile(url) =
    let uri = new Uri(url)
    let stream =
        let ext = Path.GetExtension(uri.LocalPath)
        let filePath =
            if uri.IsFile then
                uri.LocalPath
            else
                let wc = new WebClient()
                let temp = Path.GetTempFileName()
                let tempExt = Path.ChangeExtension(temp, ext)
                File.Move(temp, tempExt)
                wc.DownloadFile(uri, tempExt)
                tempExt
        if ext = ".zip" then
            let zip = new ZipFile(filePath)
            zip.GetInputStream(0L)
        else
            File.OpenRead(filePath) :> Stream
    let xdoc = XPathDocument(stream)
    let nav = xdoc.CreateNavigator()
    let mngr = new XmlNamespaceManager(new NameTable())
    let namespaces =
        [ "rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
          "rdfs", "http://www.w3.org/2000/01/rdf-schema#";
          "xsi", "http://www.w3.org/2001/XMLSchema-instance";
          "dc", "http://purl.org/dc/elements/1.1/";
          "dcterms", "http://purl.org/dc/terms/";
          "dcmitype", "http://purl.org/dc/dcmitype/";
          "cc", "http://web.resource.org/cc/";
          "pgterms", "http://www.gutenberg.org/rdfterms/" ]
    do List.iter (fun (prefix,url) -> mngr.AddNamespace(prefix,url)) namespaces
    
    let search query =
        let xpath = nav.Compile(query)
        do xpath.SetContext(mngr)
        let iter = nav.Select(xpath)
        seq { for x in iter -> x.ToString() }
    
    member x.FindIdByTitleExcat title =
        let query = Printf.sprintf "rdf:RDF/pgterms:etext[dc:title = '%s']/@rdf:ID" title
        search query
    member x.FindIdByTitle title =
        let query = Printf.sprintf "rdf:RDF/pgterms:etext[contains(dc:title,'%s')]/@rdf:ID" title
        search query

    member x.FindIdByAuthorExact title =
        let query = Printf.sprintf "rdf:RDF/pgterms:etext[dc:creator = '%s']/@rdf:ID" title
        search query
    member x.FindIdByAuthor title =
        let query = Printf.sprintf "rdf:RDF/pgterms:etext[contains(dc:creator,'%s')]/@rdf:ID" title
        search query
    
    member x.FindBookById id =
        let query = Printf.sprintf "rdf:RDF/pgterms:etext[@rdf:ID = '%s']" id
        let xpath = nav.Compile(query)
        do xpath.SetContext(mngr)
        let node = nav.SelectSingleNode(xpath)
        let res = node.MoveToFirstChild()
        let author = ref ""
        let title = ref ""
        while node.MoveToNext() do
            match node.Name with
            | "dc:title" -> title := node.Value
            | "dc:creator" -> author := node.Value
            | _ -> ()
        { Id = id;
          Title = !title;
          Author = !author; }
          
    member x.FindBookByTitleExcat title =
        let ids = x.FindIdByTitleExcat title
        Seq.map x.FindBookById ids
    member x.FindBookByTitle title =
        let ids = x.FindIdByTitle title
        Seq.map x.FindBookById ids

    member x.FindBookByAuthorExact title =
        let ids = x.FindIdByAuthorExact title
        Seq.map x.FindBookById ids
    member x.FindBookByAuthor title =
        let ids = x.FindIdByAuthor title
        Seq.map x.FindBookById ids
        
    member x.DowloadBookById id directory =
        let query = Printf.sprintf "rdf:RDF/pgterms:file[contains(dc:format/dcterms:IMT/rdf:value, 'text/plain;') and dcterms:isFormatOf/@rdf:resource = '#%s']/@rdf:about" id
        let xpath = nav.Compile(query)
        do xpath.SetContext(mngr)
        let urls = nav.Select(xpath)
        for url in urls do
            let url = url.ToString()
            //Printf.printfn "Downloading: %s" url
            let wc = new WebClient()
            let uri = new Uri(url.ToString().Replace("&", "http://www.gutenberg.org/dirs/"))
            let filename = Path.GetFileName(uri.LocalPath)
            wc.DownloadFile(uri, Path.Combine(directory, filename))
    
    member x.DownloadBookByTitleExcat title dir =
        let ids = x.FindIdByTitleExcat title
        ids |> Seq.iter (fun id -> x.DowloadBookById id dir) 
    member x.DownloadBookByTitle title dir =
        let ids = x.FindIdByTitle title
        ids |> Seq.iter (fun id -> x.DowloadBookById id dir) 

    member x.DownloadBookByAuthorExact title dir =
        let ids = x.FindIdByAuthorExact title
        ids |> Seq.iter (fun id -> x.DowloadBookById id dir) 
    member x.DownloadBookByAuthor title dir =
        let ids = x.FindIdByAuthor title
        ids |> Seq.iter (fun id -> x.DowloadBookById id dir) 
        

#if INTERACTIVE 
(*
<pgterms:file rdf:about="&f;2/6/1/4/26143/26143-8.txt">
<dc:format><dcterms:IMT><rdf:value>text/plain; charset="iso-8859-1"</rdf:value></dcterms:IMT></dc:format>
<dcterms:extent>381655</dcterms:extent>
<dcterms:modified><dcterms:W3CDTF><rdf:value>2008-07-28</rdf:value></dcterms:W3CDTF></dcterms:modified>
<dcterms:isFormatOf rdf:resource="#etext26143" />
</pgterms:file>

*)

// tests
let test = DefaultIndexUrl //@"D:\gutenberg\catalog.rdf.zip"
let index = new GutenbergIndexFile("D:\gutenberg\catalog.rdf.zip")
index.DownloadBookByAuthor "Dickens" "D:\gutenberg\dickens"
index.DowloadBookById "etext6096"  "D:\gutenberg"
//printfn "hello"
#endif