(*
 * iothub_core.ml
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

open Lwt
open Js
open XmlHttpRequest

let print s =
  Js.Unsafe.fun_call (Js.Unsafe.variable "print") [|Js.Unsafe.inject (Js.string s)|]

let log s =
  Js.Unsafe.meth_call (Js.Unsafe.variable "console") "log" [|Js.Unsafe.inject (Js.string s)|]

exception Wrong_request_type of string

let sleep d =
	let (t, w) = Lwt.task () in
	let id =
		Js.Unsafe.fun_call (Js.Unsafe.variable "setTimeout") [|Js.Unsafe.inject (
			Js.wrap_callback (fun () -> Lwt.wakeup w ())); Js.Unsafe.inject (Js.float (d *. 1000.))|]
  in
  Lwt.on_cancel t (fun () -> Js.Unsafe.fun_call (Js.Unsafe.variable "clearTimeout") [|Js.Unsafe.inject id |]);
  t

let xml_http_request request_type url json =
	let _ = 
		match request_type with
		| "GET" | "POST" | "DELETE" | "PUT" -> ()
		| _ -> raise (Wrong_request_type request_type)   
	in
	let (res, w) = Lwt.task () in
	let req = create () in
	let headers s =
		Opt.case
		(req##getResponseHeader (Js.bytestring s))
		(fun () -> None)
		(fun v -> Some (Js.to_string v))
	in
	req##_open (Js.string request_type, Js.string url, Js._true);
	req##setRequestHeader (Js.string "Content-type", Js.string ("application/json;charset=UTF-8"));
	req##onreadystatechange <- Js.wrap_callback
		(fun _ ->
			(match req##readyState with
			| DONE ->
				Lwt.wakeup w
					{url = url;
					code = req##status;
					content = Js.to_string req##responseText;
					content_xml =
						(fun () ->
							match Js.Opt.to_option (req##responseXML) with
							| None -> None
							| Some doc ->
								if (Js.some doc##documentElement) == Js.null
								then None
								else Some doc);
					headers = headers
					}
			| _ -> ()));
	(
	match json with
	| None -> req##send (Js.null)
	| Some j -> req##send(Js.some (Js.string (to_string j)))
 	);
	Lwt.on_cancel res (fun () -> req##abort ());
	res

let check_content_type ~mime_type content_type =
  match content_type with
  | Some type_
      when type_ = mime_type -> true
  | _ -> false

let http_method request_type url json =
	xml_http_request request_type url json >>= fun response ->
	let code_ = response.XmlHttpRequest.code in
	let type_ = response.XmlHttpRequest.headers "Content-type" in
	let msg = response.XmlHttpRequest.content in
	(*if (code_ = 0 || code_ = 200) && (check_content_type ~mime_type:"application/json" type_)*)
	if (code_ = 0 || code_ = 200)
	then Lwt.return (msg)
	else fst (Lwt.wait ())

let http_get url =
	http_method "GET" url None

let http_post url json =
	http_method "POST" url (Some json)