open Lwt.Infix

module Repo_id = Repo_id
module Api = Api
module App = App
module Checks = Checks  
module Installation = Installation
module Auth = Auth

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "github"

  let webhook_events_total =
    let help = "Incoming webhook events" in
    Counter.v_label ~label_name:"event" ~help ~namespace ~subsystem "webhook_events_total"
end

let webhook : Current_web.Resource.t = object 
  inherit Current_web.Resource.t

  method! post_raw _site req body =
    Log.info (fun f -> f "input_webhook: %a" Cohttp_lwt.Request.pp_hum req);
    let headers = Cohttp.Request.headers req in
    let event = Cohttp.Header.get headers "X-GitHub-Event" in
    Log.info (fun f -> f "Got GitHub event %a" Fmt.(option ~none:(any "NONE") (quote string)) event);
    Prometheus.Counter.inc_one (Metrics.webhook_events_total (Option.value event ~default:"NONE"));
    Cohttp_lwt.Body.to_string body >|= Yojson.Safe.from_string >>= fun body ->
    begin match event with
      | Some "installation_repositories" -> Installation.input_installation_repositories_webhook ()
      | Some "installation" -> App.input_installation_webhook ()
      | Some ("pull_request" | "push" | "create") -> Api.input_webhook body
      (* TODO Add hook for check_suite to get triggered here! *)
      | Some ("check_suite") -> Checks.check_suite_webhook body
      (* TODO Add hook for check_run to get triggered here! *)

      | Some x -> Log.warn (fun f -> f "Unknown GitHub event type %S" x)
      | None -> Log.warn (fun f -> f "Missing GitHub event type in webhook!")
    end;
    Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()
end
