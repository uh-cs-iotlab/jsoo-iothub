(* Dashboard for the IoT hub made with jsoo *)

open Lwt
open Js
open Yojson.Safe
open Deriving_Yojson
open XmlHttpRequest

let (>>=) = Lwt.bind

let js = Js.string
let document = Dom_html.window##document

exception Wrong_request_type of string

type plugin = {
  pluginType : string;
  id : int;
	serviceName : string;
	packageName : string;
} deriving (Yojson)

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

let get_plugins () =
	http_get ("plugins/") >|= (fun msg ->
		try
			let l = Yojson.from_string<plugin list> msg in
				Some l
			with
			| Expected_type (s, json) ->
				(*let errMsg = "Error in processing the json response, expected type was " ^ s ^ " and got " ^ (to_string json) in
				debug (Scanf.format_from_string errMsg);*)
				None
			| _ ->
				(*debug "Error in processing the json response, unknown exception";*)
				None
	)

let get_feeds () =
	http_get ("feeds/") >|= (fun msg ->
		try
			let l = Yojson.from_string<feed list> msg in
			Some l
		with
		| Expected_type (s, json) -> None
		| _ -> None
	)

let plugins_elements () =
	let d =  Dom_html.window##document in
	let plugins = Dom_html.createDiv d in
	plugins##style##border <- Js.string "1px black solid";
	let t = Dom_html.createH2 d in
	Dom.appendChild t (d##createTextNode (js "Plugins"));
	Dom.appendChild plugins t;
	let ul = Dom_html.createUl d in
	let plugin_to_li plugin =
		let li = Dom_html.createLi d in
		let s = plugin.pluginType ^ ": " ^ plugin.serviceName in
		Dom.appendChild li (d##createTextNode (js s))
	in
	ignore(get_plugins () >>= (fun pl ->
	(match pl with
	| Some (a::l) -> List.iter plugin_to_li (a::l)
	| _ -> let li = Dom_html.createLi d in
		Dom.appendChild li (d##createTextNode (js "No plugins found")); 
		Dom.appendChild ul li; ()
	);Lwt.return_unit));
	Dom.appendChild plugins ul;
	plugins

let feeds_elements () =
	let d =  Dom_html.window##document in
	let plugins = Dom_html.createDiv d in
	plugins##style##border <- Js.string "1px black solid";
	let t = Dom_html.createH2 d in
	Dom.appendChild t (d##createTextNode (js "Feeds"));
	Dom.appendChild plugins t;
	let ul = Dom_html.createUl d in
	let plugin_to_li plugin =
		let li = Dom_html.createLi d in
		let s = plugin.pluginType ^ ": " ^ plugin.serviceName in
		Dom.appendChild li (d##createTextNode (js s))
	in
	ignore(get_feeds () >>= (fun pl ->
	(match pl with
	| Some (a::l) -> List.iter plugin_to_li (a::l)
	| _ -> let li = Dom_html.createLi d in
		Dom.appendChild li (d##createTextNode (js "No plugins found")); 
		Dom.appendChild ul li; ()
	);Lwt.return_unit));
	Dom.appendChild plugins ul;
	plugins
	
let create_title title =
	let d =  Dom_html.window##document in
	let t = Dom_html.createH1 d in
	Dom.appendChild t (Dom_html.window##document##createTextNode (Js.string title));
	t

let create_dashboard () =
	let d =  Dom_html.window##document in
	let dashboard = Dom_html.createDiv d in
	let title = create_title "Welcome to your Iot Hub dashboard!" in
	Dom.appendChild Dom_html.window##document##body title;
	let my_plugins_elements = plugins_elements () in
	Dom.appendChild dashboard my_plugins_elements;
  dashboard

let start _ =
	let dashboard = create_dashboard () in
	Dom.appendChild document##body dashboard;
	Js._false

let _ =
	Dom_html.window##onload <- Dom_html.handler start
