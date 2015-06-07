(*
 * last_call_light.ml
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

open Iothub_data_model
open Iothub_core
open Lwt
open Js

let get url =
	http_get url

let inner_time_diff before after : number t = 
	Js.Unsafe.fun_call (Js.Unsafe.variable "native_time_diff") 
		[|Js.Unsafe.inject (Js.string before); Js.Unsafe.inject (Js.string after)|]
		
let time_diff before after =
	let d = inner_time_diff before after in
	float_of_number d

let fade feed =
	let jsin = Js.string "{\"light\": {\"luminosity\":1, \"fade\": 90}}" in
	let jsout = Js.string "{\"light\": {\"luminosity\":100, \"fade\": 90}}" in
	Js.Unsafe.fun_call (Js.Unsafe.variable "native_dim") 
		[|Js.Unsafe.inject (Js.string feed); Js.Unsafe.inject jsin; Js.Unsafe.inject jsout|]

let check_room events_feed room_light_feed =
	let get_time () = (jsnew date_now ())##toString() in
	let curr_time = get_time() in
	let needToDimOneP period =
		match period with
		| `Period (`Date(s,_),`Date(e,_)) ->
			let de = time_diff (Js.to_string curr_time) e  in
			let ds = time_diff s (Js.to_string curr_time) in
			de > 0. && ds > 0.0
		| _ -> false
	in
	let needToDim events =
		match events with
		| `Series s -> List.exists (fun p -> 
			begin match p with
			| `Period _ -> needToDimOneP p
			| _ -> false
			end
			) s
		| `Period _ -> needToDimOneP events
		| _ -> false
	in
	get events_feed >>= (fun periods ->
		let json = Yojson.Basic.from_string periods in
		let events = iothub_data_from_json json in
		begin 
			if needToDim events 
			then
				 	fade room_light_feed
			else
				print ("No need to dim the lights")
		end;
		Lwt.return ()
	)

let run () =
	let interval = 5. in
	let eventsFeed = "http://127.0.0.1:8081/feeds/atomicFeature1" in
	let room_light_feed = "http://127.0.0.1:8081/feeds/atomicFeature2" in
	let rec f () =
		ignore(check_room eventsFeed room_light_feed);
		sleep interval >>= f
	in ignore(f())

let _ =
	Js.Unsafe.global##lastCallLightService <- jsobject
    method needConfiguration = Js._true
		method checkConfiguration config = Js._true
		method run = run
  end
