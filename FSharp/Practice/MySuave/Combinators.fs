module Combinators

let compose first second context =
    async {
        let! firstContext = first context
        match firstContext with
        | None -> return None
        | Some context ->
            let! secondContext = second context
            return secondContext
    }

let (>=>) = compose

