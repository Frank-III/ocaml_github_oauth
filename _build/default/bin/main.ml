[@@@ocaml.warning "-32-33-27"]

open Base

type github_user = 
  { id: int 
  ; login: string
  } [@@deriving show, yojson {strict = false}] 


let get_required_env var = 
  match Stdlib.Sys.getenv var with 
  | "" -> Fmt.failwith "Empty $%s" var
  | value -> value
  | exception _ -> Fmt.failwith "Missing $%s" var

let get_github_user token = 
    let open Lwt.Syntax in 
    let* response, body = 
      Cohttp_lwt_unix.Client.get 
        (Uri.of_string "https://api.github.com/user")
        ~headers:(Cohttp.Header.of_list [("Authorization", Fmt.str "Bearer %s" token); ("Accept", "application/json"); ("User-Agent", "ocaml-dream")])
    in 
    let* body = Cohttp_lwt.Body.to_string body in 
    match Cohttp.Response.status response with 
    | `OK -> 
      Dream.log "%s" body;
      let json = Yojson.Safe.from_string body in 
      let user = github_user_of_yojson json in 
      Lwt.return user
      (* Too dumb, how to deserialize in Ocaml *)
      (* let id = Yojson.Safe.Util.(json |> member "id" |> to_int) in
      let login = Yojson.Safe.Util.(json |> member "login" |> to_string) in
      Lwt.return @@ Ok { id; login } *)
    | _ -> 
      Fmt.failwith "Failed to get user info: %s" body

let gen_random_string length = 
  let open Base in 
  let rec aux acc = function 
    | 0 -> acc 
    | n -> aux (acc ^ Char.to_string @@ Random.ascii ()) (n - 1) in 
  aux "" length

  
let handle_authorization req = 
  let open Lwt.Syntax in
  let client_id = get_required_env "GITHUB_CLIENT_ID" in
  let state = gen_random_string 41 in 
  let params = 
    [ ("client_id", client_id)
    ; ("state", state)
    ; ("response_type", "code")
    ] in
  (* let state_cookie = Fmt.str "state=%s; Path=/; Max-Age=3600; HttpOnly" state in *)
  let url = Uri.add_query_params' (Uri.of_string "https://github.com/login/oauth/authorize") params in
  let response = Dream.response ~code:302 ~headers:[("Location", Uri.to_string url)] "" in 
  begin 
    Dream.set_cookie ~max_age:3600. ~http_only:true ~path:(Some "/") response req "state" state |> ignore ;
    Lwt.return response
  end


type access_token_result = 
  { access_token: string
  ; token_type: string
  ; scope: string
  } [@@deriving show, yojson]


let get_access_token = function
  | Ok result -> result.access_token
  | Error err -> 
    (* Dream.log "%s" result *)
    Fmt.failwith "Failed to extract authorization code: %s" err

let exchange_authorization_code = function
  | None -> 
    Fmt.failwith "Missing authorization code"
  | Some code -> 
    let open Lwt.Syntax in 
    let client_id = get_required_env "GITHUB_CLIENT_ID" in
    let client_secret = get_required_env "GITHUB_CLIENT_SECRET" in
    let params = 
      [ ("client_id", [client_id])
      ; ("client_secret", [client_secret])
      ; ("grant_type", ["code"])
      ; ("code", [code])
      ] in
    let* resp, body = 
      Cohttp_lwt_unix.Client.post_form 
      ~params: params
      ~headers:(Cohttp.Header.of_list [("Accept", "application/json")])
      (Uri.of_string "https://github.com/login/oauth/access_token") in 
    match Cohttp.Response.status resp with
    | `OK -> 
      let* body = Cohttp_lwt.Body.to_string body in 
      Dream.log "%s" body;
      let json = Yojson.Safe.from_string body in 
      let result = access_token_result_of_yojson json in 
      Lwt.return @@ get_access_token result
    | _ -> 
      Fmt.failwith "Failed to exchange authorization code: %s" (Cohttp.Response.status resp |> Cohttp.Code.string_of_status)


let handle_callback req =
  let state = Dream.query req "state" in 
  let state_cookies = Dream.cookie req "state" in
  match (state, state_cookies) with
  | Some state, Some state_cookies when String.(state = state_cookies) -> (
    let code = Dream.query req "code" in 
    let open Lwt.Syntax in 
    let* access_token = exchange_authorization_code code in 
    (* how to do it here, possible to do a monad ? *)
    let* user = get_github_user access_token in 
    match user with
    | Ok { id; login } ->
      Dream.html @@ Fmt.str "Hello, %s!" login
    | _ ->
      Fmt.failwith "Failed to get user info")
  | _ -> 
    Dream.log "state: %s, cookie_state: %s" (Option.value ~default:"" state) (Option.value ~default:"" state_cookies);
    Lwt.return @@ Dream.response ~code:403 ""

let () = 
  Dotenv.export() |> ignore;
  Dream.run ~port:3000 @@
  Dream.logger @@
  Dream.router [
    (Dream.get "/" @@ fun _ -> 
      Dream.html "Good morning, world!") ;
    Dream.get "/auth/login/github" @@ handle_authorization ; 
    Dream.get "/auth/login/github/callback" @@ handle_callback ;
  ]


