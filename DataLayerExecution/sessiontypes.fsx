 // Basic Operations
[<AbstractClass>]
type Ops = class end
type Eps = class inherit Ops end 
type Send<'T, 'Rest when 'Rest :> Ops> = class inherit Ops end 
type Recv<'T, 'Rest when 'Rest :> Ops> = class inherit Ops end 
type Choose<'Left, 'Right when 'Left :> Ops and 'Right :> Ops> = class inherit Ops end 
type Offer<'Left, 'Right when 'Left :> Ops and 'Right :> Ops> = class inherit Ops end

// Session Parameterized Monad
type Msg = Msg of (obj * AsyncReplyChannel<unit>)
// session contains phantom types describing both its own and dual structure
type Session<'S1, 'S2, 'RS1, 'RS2, 'T> = Session of (MailboxProcessor<Msg> -> Async<'T>) 
type SessionBuilder() = 
    member self.Return (value : 'T) : Session<'S, 'S, 'RS, 'RS, 'T> = 
        Session (fun _ -> async { return value })
    member self.Bind (session : Session<'S1, 'S2, 'RS1, 'RS2, 'T>, f : 'T -> Session<'S2, 'S3, 'RS2, 'RS3, 'R>) 
        : Session<'S1, 'S3, 'RS1, 'RS3, 'R> =
        Session (fun mailBox ->
            async {
                let (Session r) = session
                let! x = r mailBox
                let (Session r') = f x
                return! r' mailBox
            }) 

let session = new SessionBuilder()

// Basic Operations
let send (value : 'T) : Session<Send<'T, 'Rest>, 'Rest, Recv<'T, 'RRest>, 'RRest, unit> = 
    Session (fun mailBox -> 
        async { return! mailBox.PostAndAsyncReply (fun reply -> Msg (value :> obj, reply)) })

let recv () : Session<Recv<'T, 'Rest>, 'Rest, Send<'T, 'RRest>, 'RRest, 'T> = 
    Session (fun mailBox -> 
        async { 
            let! (Msg (value, reply)) = mailBox.Receive()
            reply.Reply ()
            return value :?> 'T 
        })

let sel1 () : Session<Choose<'First, 'Second>, 'First, Offer<'RFirst, 'RSecond>, 'RFirst, unit> =
    Session (fun mailBox -> 
        async { return! mailBox.PostAndAsyncReply (fun reply -> Msg (true :> obj, reply)) })

let sel2 () : Session<Choose<'First, 'Second>, 'Second, Offer<'RFirst, 'RSecond>, 'RSecond, unit> =
    Session (fun mailBox -> 
        async { return! mailBox.PostAndAsyncReply (fun reply -> Msg (false :> obj, reply)) })

let cases (left : Session<'First, 'Rest, 'RFirst, 'RRest, 'T>) (right : Session<'Second, 'Rest, 'RSecond, 'RRest, 'T>) 
    : Session<Offer<'First, 'Second>, 'Rest, Choose<'RFirst, 'RSecond>, 'RRest, 'T> = 
    Session (fun mailBox -> 
        async {
            let! (Msg (value, reply)) = mailBox.Receive()
            reply.Reply ()
            match value :?> bool with
            | true ->
                let (Session r) = left
                return! r mailBox 
            | false ->
                let (Session r) = right
                return! r mailBox
        })

let run (client : Session<'Client, Eps, 'Server, Eps, 'T>)
        (server : Session<'Server, Eps, 'Client, Eps, unit>) = 
    let mailBox = MailboxProcessor.Start(fun _ -> async { () })
    let (Session r) = server
    let (Session r') = client
    let result  = 
        [|async { let! _ = r mailBox in return Unchecked.defaultof<'T> }; r' mailBox|] 
        |> Async.Parallel |> Async.RunSynchronously 
    result.[1]

let clientAdd() = 
    session {
        do! send 1
        do! send 2
        let! result = recv ()
        return result
    }


let serverAdd() = 
    session {
        let! first = recv ()
        let! second = recv ()
        do! send (first + second)
    }

let clientStringToInt() = 
    session {
        do! send "42"
        let! result = recv ()
        return result
    }

let serverStringToInt() = 
    session {
        let! value = recv ()
        do! send (System.Int32.Parse value)
    }
    
let client (input : int) = 
    session {
        if input = 1 then
            do! sel1 ()
            let! value = clientAdd()
            return value
        else
            do! sel2 ()
            let! value = clientStringToInt()
            return value
    }

let server () =
    cases (serverAdd()) (serverStringToInt())

run  (clientAdd()) (serverAdd()) // 3
run  (clientStringToInt()) (serverStringToInt()) // 42
run  (client 1) (server()) // 3
run  (client 2) (server()) // 42