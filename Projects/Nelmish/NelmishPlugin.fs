﻿namespace Nelmish
open Nu
open Nelmish

// this is a plugin for the Nu game engine that directs the execution of your application and editor
type NelmishPlugin () =
    inherit NuPlugin ()