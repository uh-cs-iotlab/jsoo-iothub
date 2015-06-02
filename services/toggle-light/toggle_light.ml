(*
 * toggle_light.ml
 * v0.1
 * 2015
 *
 * Copyright 2015 University of Helsinki
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License. 
 * You may obtain a copy of the License at:
 * 	http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, 
 * software distributed under the License is distributed on an 
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, 
 * either express or implied.
 * See the License for the specific language governing permissions 
 * and limitations under the License.
 * 
 * Author: Julien Mineraud <julien.mineraud@cs.helsinki.fi>
 *)

open Iothub_core

let name = "toggle-light-service"

(* define the interface of toggle-light-service *)
class type toggleLightService = object
  method needConfiguration : (unit -> bool) Js.callback Js.writeonly_prop
end

let needConfiguration () =
	false 

let _ =
  let need = Js.wrap_callback needConfiguration in
  let open Js.Unsafe in
	let methodArray =  [|("needConfiguration", inject need)|](*; ("abs", inject abs); ("zero", inject zero)|];*) in
  global##toggleLightService <- obj methodArray

let get url =
	http_get url

