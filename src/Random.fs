namespace Fsharp21
  open System

  module Random =
    let randomFactory (rand:Random) _ =
      rand.Next ()

    let random _ =
      randomFactory (Random()) ()