(* 
 * Google calendar plugin for the IoT hub made with jsoo
 * I would like to have all the types of connections supported by this library 
 * but I would concentrate on the public key access ATM
 *)


open Lwt
open Js
open XmlHttpRequest

let (>>=) = Lwt.bind

let js = Js.string
let document = Dom_html.window##document

exception Wrong_request_type of string

let url = "url"

(*let _ =
  let configure = Js.wrap_callback (fun _ ->  x +. y) in
  let abs = Js.wrap_callback (fun x -> abs_float x) in
  let zero = 0. in
  let open Js.Unsafe in
  global##mylib <-
    obj [|("add", inject add); ("abs", inject abs); ("zero", inject zero)|];*)

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
	(match json with
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
	if (code_ = 0 || code_ = 200) && (check_content_type ~mime_type:"application/json" type_)
	then Lwt.return (msg)
	else fst (Lwt.wait ())

let http_get url =
	http_method "GET" url None

let http_post url json =
	http_method "POST" url (Some json)

let create_title title =
	let d =  Dom_html.window##document in
	let t = Dom_html.createH1 d in
	Dom.appendChild t (Dom_html.window##document##createTextNode (Js.string title));
	t

let create_dashboard () =
	let d =  Dom_html.window##document in
	let dashboard = Dom_html.createDiv d in
	let title = create_title "Test of google calendar!" in
	Dom.appendChild Dom_html.window##document##body title;
	ignore(http_get (url) >>= (fun response ->
		Dom.appendChild dashboard (Dom_html.window##document##createTextNode (Js.string response)); 
		Lwt.return_unit));
  dashboard

let start _ =
	let dashboard = create_dashboard () in
	Dom.appendChild document##body dashboard;
	Js._false

let _ =
	Dom_html.window##onload <- Dom_html.handler start
