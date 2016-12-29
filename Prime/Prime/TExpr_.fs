// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System

type Texpr<'a, 'env> = 'env -> 'a * 'env

type TexprBuilder<'env> () =

    member inline this.Bind (expr : Texpr<'a, 'env>, lift : 'a -> Texpr<'b, 'env>) : Texpr<'b, 'env> =
        fun env ->
            let (result, env') = expr env
            let expr' = lift result
            expr' env'

    member inline this.Return (value : 'a) : Texpr<'a, 'env> =
        fun expr ->
            (value, expr)

    member inline this.ReturnFrom (value : 'a) =
        value

    member this.Zero () =
        this.Return ()
        
    member this.Combine (l, r) =
        this.Bind (l, fun () -> r)
        
    member this.TryWith (body : Texpr<'a, 'expr>, handler : exn -> Texpr<'a, 'expr>) : Texpr<'a, 'expr> =
        fun env ->
            try body env
            with exn -> handler exn env

    member this.TryFinally (body : Texpr<'a, 'expr>, compensation) : Texpr<'a,'expr> =
        fun env ->
            try body env
            finally compensation()

    member this.Using (res : #IDisposable, body) =
        this.TryFinally (body res, (fun () ->
            match res with null -> () | disp -> disp.Dispose()))

    member this.Delay f =
        this.Bind (this.Return (), f)

    member this.While (guard, body) =
        if not ^ guard ()
        then this.Zero ()
        else this.Bind (body, (fun () -> this.While (guard, body)))

    member this.For (seq : _ seq, body) =
        this.Using (seq.GetEnumerator (), (fun enr ->
            this.While (enr.MoveNext, this.Delay (fun () ->
                body enr.Current))))