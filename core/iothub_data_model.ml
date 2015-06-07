open Yojson.Safe

exception Unknown_data_unit of string
exception Unknown_data_format of string
exception Unknown_iothub_data
exception Not_yet_implemented
exception Wrong_iothub_data of string
exception Wrong_field_data of string

(*** Data description ***)

(* this type is the very basic data type description *)
type iothub_data_description = [
	| `String
	| `Float
	| `Int
	| `Bool
	| `Date
	| `Period ]

type iothub_date_format = [ `ISO_8601 | `RFC_3339 ]

let date_format_to_string = function
	| `ISO_8601 -> "ISO 8601"
	| `RFC_3339 -> "RFC 3339"

let date_format_from_string = function
	| "ISO 8601" -> `ISO_8601
	| "RFC 3339" -> `RFC_3339
	| _ -> raise(Unknown_data_format "date")

type iothub_date_data = [ `Date of string * iothub_date_format ]

type iothub_data = [
	| `Light of int * float
	| `String of string
	| `Float of float
	| `Int of int
	| `Bool of bool
	| iothub_date_data
	| `Period of iothub_date_data * iothub_date_data
	| `Series of iothub_data list ]

(** feed functions **)

type field_data = {
	name : string;
	data : iothub_data;
}

type feed_data = [
	| `Atomic of iothub_data
	| `Composed of field_data list
	| `Executable ]

let iot_hub_date_data_to_json = function
	| `Date (d,f) -> 
		let _d = ("time", `String d) in
		let _f = ("format", `String (date_format_to_string f)) in
		`Assoc [("date", `Assoc [_d; _f])]
	| _ -> raise (Wrong_iothub_data "Data")
									
(** utility functions **)
let rec iothub_data_to_json = function
	| `String s -> `Assoc [("string", `String s)]
	| `Float f -> `Assoc [("float", `Float f)]
	| `Int i -> `Assoc [("int", `Int i)]
	| `Bool b -> `Assoc [("bool", `Bool b)]
	| `Light (l,f) -> `Assoc [("light", `Assoc [("luminosity", `Int l); ("fade", `Float f)])]
	| `Date d -> iot_hub_date_data_to_json (`Date d)
	| `Period (s, e) -> 
		let _s = ("start", iot_hub_date_data_to_json s) in
		let _e = ("end", iot_hub_date_data_to_json e) in
		`Assoc [("period", `Assoc [_s; _e])]
	| `Series s ->
		`List (List.map (fun _a -> iothub_data_to_json _a) s)
	| _ -> raise Unknown_iothub_data

let date_data_from_json json =
	let open Yojson.Basic.Util in
	let d_json = json |> member "date" in
	match d_json with
	| `Assoc _ ->
		let time = match d_json |> member "time" with
		| `String s -> s
		| _ -> raise(Wrong_iothub_data "data") in
		let format = match d_json |> member "format" with
		| `String s -> date_format_from_string s
		| _ -> raise(Wrong_iothub_data "data") in
		`Date(time, format)
	| _ -> raise(Wrong_iothub_data "date")

let period_data_from_json json =
	let open Yojson.Basic.Util in
	let p_json = json |> member "period" in
	match p_json with
	| `Assoc _ ->
		let s = match p_json |> member "start" with
		| `Assoc d -> date_data_from_json (p_json |> member "start")
		| _ -> raise(Wrong_iothub_data "period") in
		let e = match p_json |> member "end" with
		| `Assoc d -> date_data_from_json (p_json |> member "end")
		| _ -> raise(Wrong_iothub_data "period") in
		`Period (s, e)
	| _ -> raise(Wrong_iothub_data "period")

let light_data_from_json json =
	let open Yojson.Basic.Util in
	let p_json = json |> member "light" in
	match p_json with
	| `Assoc _ ->
		let l = match p_json |> member "luminosity" with
		| `Int l' -> l'
		| _ -> raise(Wrong_iothub_data "luminosity") in
		let f = match p_json |> member "fade" with
		| `Assoc f' -> f'
		| _ -> raise(Wrong_iothub_data "luminosity") in
		`Light (l, f)
	| _ -> raise(Wrong_iothub_data "luminosity")

let rec iothub_data_from_json json =
	match json with
	| `List a -> `Series (List.map (fun j -> iothub_data_from_json j) a)
	| _ ->
		let open Yojson.Basic.Util in
		if json |> member "period" != `Null then 
			period_data_from_json json
		else if json |> member "date" != `Null then 
			date_data_from_json json
		else if json |> member "light" != `Null then
			light_data_from_json json
		else if json |> member "string" != `Null then
			let v = match json |> member "string" with
			| `String v' -> v'
			| _ -> raise(Wrong_iothub_data "string") 	
			in `String(v) 
		else if json |> member "float" != `Null then
			let v = match json |> member "float" with
			| `Float v' -> v'
			| _ -> raise(Wrong_iothub_data "float") 	
			in `Float(v)
		else if json |> member "int" != `Null then
			let v = match json |> member "int" with
			| `Int v' -> v'
			| _ -> raise(Wrong_iothub_data "int") 	
			in `Int(v)
		else if json |> member "bool" != `Null then
			let v = match json |> member "bool" with
			| `Bool v' -> v'
			| _ -> raise(Wrong_iothub_data "bool") 	
			in `Bool(v)
		else raise(Unknown_iothub_data) 

let field_data_to_json (fd:field_data) =
	let jname = ("name", `String fd.name) in
	let jdata = ("data", iothub_data_to_json fd.data) in
	`Assoc [jname; jdata]

let feed_data_to_json = function
	| `Atomic data -> iothub_data_to_json data
	| `Composed l -> 
		begin match l with
		| a::[] -> field_data_to_json a
		| _ -> `List (List.map field_data_to_json l)
		end
	| `Executable -> raise Not_yet_implemented
