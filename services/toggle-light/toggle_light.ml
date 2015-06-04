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

(* open Iothub_core

let name = "toggle-light-service" *)

open Lwt
open Js

(* define the interface of toggle-light-service *)
(*class type toggleLightService = object
  method needConfiguration : (unit -> bool) Js.callback Js.readonly_prop
end

let needConfiguration () =
	true 
 let get url =
	http_get url *)
	
let print s =
  Js.Unsafe.fun_call (Js.Unsafe.variable "print") [|Js.Unsafe.inject (Js.string s)|]
	
let log s =
  Js.Unsafe.meth_call (Js.Unsafe.variable "console") "log" [|Js.Unsafe.inject (Js.string s)|]

let sleep d =
	let (t, w) = Lwt.task () in

let startFunction () =
	let interval = 1000. in
	let get_time () = (jsnew date_now ())##toString() in
	let rec f () =
		let time = get_time() in
		print ("Hello from toggle_light.ml at " ^ (Js.to_string time));
		Lwt_js.sleep interval >>= f
	in ignore(f())

let _ =
	Js.Unsafe.global##toggleLightService <- jsobject
    method needConfiguration () = Js._true
		method start () = startFunction ()
		
  end
  (*let need = Js.wrap_callback needConfiguration in
  let open Js.Unsafe in
	let methodArray =  [|("needConfiguration", inject need)|](*; ("abs", inject abs); ("zero", inject zero)|];*) in
  global##toggleLightService <- obj methodArray;
	global##toggleLightService;
	()*)



