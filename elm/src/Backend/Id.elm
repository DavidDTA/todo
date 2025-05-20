module Backend.Id exposing (generate)

import Backend.Interop
import BinaryBase64
import ConcurrentTask


generate tag =
    Backend.Interop.getRandom 9
        |> ConcurrentTask.map (BinaryBase64.encode >> String.replace "+" "-" >> String.replace "/" "_" >> tag)
