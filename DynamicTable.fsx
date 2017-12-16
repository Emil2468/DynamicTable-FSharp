type DynamicTable<'T>() =
    let mutable (data : 'T[]) = Array.create 1 Unchecked.defaultof<'T>
    let mutable index = 0
    member this.Type =  typeof<'T>
    member this.Data with get() = data
    member this.Index = index
    member this.Push(value : 'T) =
        if this.Index < this.Data.Length then
            data.[this.Index] <- value
        else
            this.AddSection()
            data.[this.Index] <- value
        index <- index + 1

    member this.Pop() =
        this.Data.[index - 1] <- Unchecked.defaultof<'T>
        index <- index - 1
        if index <= this.Data.Length / 4 then
            this.RemoveSection()
    member this.AddSection() =
        let mutable temp = Array.create (this.Data.Length * 2) Unchecked.defaultof<'T>
        for i = 0 to data.Length - 1 do
            temp.[i] <- data.[i]
        data <- Array.copy temp
    member this.RemoveSection() =
        let mutable temp = Array.create (this.Data.Length / 2) Unchecked.defaultof<'T>
        for i = 0 to temp.Length / 2 - 1 do
            temp.[i] <- data.[i]
        data <- Array.copy temp
    override this.ToString() = sprintf "%A" data


(*let d = DynamicTable<float>()
printfn "%A" d
printfn "Tilføj værdi 10.0:"
d.Push(10.0)
printfn "%A" d
printfn "Tilføj værdi 10.5:"
d.Push(10.5)
printfn "%A" d
printfn "Tilføj værdi 10.3:"
d.Push(10.3)
printfn "%A" d
printfn "Tilføj værdi 15.5:"
d.Push(15.5)
printfn "%A" d
printfn "Tilføj værdi 14.5:"
d.Push(14.5)
printfn "%A" d

printfn "\nGet index 4"
printfn "%A" d.Data.[4]
printfn "Get index 2"
printfn "%A" d.Data.[2]
printfn "Get index 0"
printfn "%A" d.Data.[0]

printfn "\nPop"
d.Pop()
printfn "%A" d
printfn "Pop"
d.Pop()
printfn "%A" d
printfn "Pop"
d.Pop()
printfn "%A" d
printfn "Pop"
d.Pop()
printfn "%A" d
printfn "Pop"
d.Pop()
printfn "%A" d*)
