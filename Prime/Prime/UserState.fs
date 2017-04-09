namespace Prime
open System
open System.ComponentModel
open System.Reflection

[<AutoOpen>]
module UserStateModule =

    type UserStateConverter () =
        inherit TypeConverter ()
        
        override this.CanConvertTo (_, destType) =
            destType = typeof<Symbol> ||
            destType = typeof<UserState>

        override this.ConvertTo (_, _, source, destType) =
            if destType = typeof<Symbol> then
                let userState = source :?> UserState
                let typeSymbol = Symbol.String (userState.Type.AssemblyQualifiedName, None)
                let valueConverter = SymbolicConverter userState.Type
                let valueSymbol = valueConverter.ConvertTo (userState.Value, typeof<Symbol>) :?> Symbol
                let imperativeConverter = SymbolicConverter typeof<bool>
                let imperativeSymbol = imperativeConverter.ConvertTo (userState.Imperative, typeof<Symbol>) :?> Symbol
                Symbols ([typeSymbol; valueSymbol; imperativeSymbol], None) :> obj
            elif destType = typeof<UserState> then source
            else failconv "Invalid UserState conversion to source." None

        override this.CanConvertFrom (_, sourceType) =
            sourceType = typeof<Symbol> ||
            sourceType = typeof<UserState>

        override this.ConvertFrom (_, _, source) =
            match source with
            | :? Symbol as symbol ->
                match symbol with
                | Symbols ([String (typeName, _); valueSymbol; imperativeSymbol], _) ->
                    let valueType = Type.GetType (typeName, true, false)
                    let valueConverter = SymbolicConverter valueType
                    let value = valueConverter.ConvertFrom valueSymbol; 
                    let imperativeConverter = SymbolicConverter typeof<bool>
                    let imperative = imperativeConverter.ConvertFrom imperativeSymbol :?> bool
                    { Type = valueType; Value = value; Imperative = imperative } :> obj
                | _ -> failconv "Invalid UserState conversion from source." ^ Some symbol
            | :? UserState -> source
            | _ -> failconv "Invalid UserState conversion from source." None

    /// User-defined state.
    and [<NoComparison; TypeConverter (typeof<UserStateConverter>)>] UserState =
        private
            { mutable Type : Type
              mutable Value : obj
              Imperative : bool }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module UserState =

        /// Check that the user state is imperative.
        let getImperative userState =
            userState.Imperative

        /// Get the state.
        let get<'a> userState : 'a =
            userState.Value :?> 'a

        /// Set the state.
        let set<'a> (value : 'a) userState =
            if userState.Imperative then
                userState.Type <- typeof<'a>
                userState.Value <- value
                userState
            else
                { userState with
                    Type = typeof<'a>
                    Value = value }

        let update (updater : 'a -> 'b) userState =
            let value = get userState
            set (updater value) userState

        /// Make UserState.
        let make (value : 'a) imperative =
            { Type = typeof<'a>
              Value = value
              Imperative = imperative }

/// User-defined state.
type UserState = UserStateModule.UserState