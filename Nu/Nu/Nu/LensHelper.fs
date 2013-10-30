module Nu.LensHelper
                        
let getChild childFinder parent address =
    match address with
    | [head] -> childFinder head parent
    | _ -> failwith ("Invalid address '" + str address + "'.")

let setChild childAdder parent address child =
    match address with
    | [head] -> childAdder head parent child
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getChildSem childFinder childToSem address parent =
    let child = getChild childFinder parent address
    let semantic = childToSem child
    (child, semantic)

let setChildSem childAdder childSemSetter address parent child semantic =
    let child2 = childSemSetter child semantic
    setChild childAdder parent address child2

let getChildSemSem childFinder childToSemSem address parent =
    let child = getChild childFinder parent address
    let (semantic, semantic2) = childToSemSem child
    (child, semantic, semantic2)

let setChildSemSem childAdder childSemSemSetter address parent child semantic semantic2 =
    let child2 = childSemSemSetter child semantic semantic2
    setChild childAdder parent address child2

let getOptChild optChildFinder parent address =
    match address with
    | [] -> None
    | [head] ->
        let optChild = optChildFinder head parent
        match optChild with
        | None -> None
        | Some child -> Some child
    | _ :: _ -> None

let setOptChild addChild removeChild parent address optChild =
    match address with
    | [head] ->
        match optChild with
        | None -> removeChild head parent
        | Some child -> addChild head parent child
    | _ -> failwith ("Invalid address '" + str address + "'.")

let getOptChildSem optChildFinder childToOptSem parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optSem = childToOptSem child
        match optSem with
        | None -> None
        | Some semantic -> Some (child, semantic)

let setOptChildSem childAdder childRemover childSemSetter optChildSem parent address =
    match optChildSem with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, semantic) -> setChildSem childAdder childSemSetter address parent child semantic
    
let getOptChildSemSem optChildFinder childToOptSemSem parent address =
    let optChild = getOptChild optChildFinder parent address
    match optChild with
    | None -> None
    | Some child ->
        let optSemSem = childToOptSemSem child
        match optSemSem with
        | None -> None
        | Some (semantic, semantic2) -> Some (child, semantic, semantic2)

let setOptChildSemSem childAdder childRemover childSemSemSetter optChildSemSem parent address =
    match optChildSemSem with
    | None -> setOptChild childAdder childRemover parent address None
    | Some (child, semantic, semantic2) -> setChildSemSem childAdder childSemSemSetter address parent child semantic semantic2

