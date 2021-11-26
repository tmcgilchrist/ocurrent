open Lwt.Infix

module Repo_id = Repo_id
module Api = Api

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "gitlab"

  let webhook_events_total =
    let help = "Incoming webhook events" in
    Counter.v_label ~label_name:"event" ~help ~namespace ~subsystem "webhook_events_total"
end

(* Surprisingly this is a string compare, no signing involved.
   https://docs.gitlab.com/ee/user/project/integrations/webhooks.html#secret-token
*)
let validate_webhook webhook_secret headers =
  let request_header = Option.value ~default:"<empty>" @@ Cohttp.Header.get headers "X-Gitlab-Token" in
  Eqaf.equal request_header webhook_secret

let webhook ~webhook_secret = object
    inherit Current_web.Resource.t

    method! post_raw _site req body =
      Log.info (fun f -> f "input_webhook: %a" Cohttp_lwt.Request.pp_hum req);
      let headers = Cohttp.Request.headers req in
      let event = Cohttp.Header.get headers "X-Gitlab-Event" in
      Log.info (fun f -> f "Got GitLab event %a" Fmt.(option ~none:(any "NONE") (quote string)) event);
      Prometheus.Counter.inc_one (Metrics.webhook_events_total (Option.value ~default:"NONE" event));
      Cohttp_lwt.Body.to_string body >>= fun body ->
      match validate_webhook webhook_secret headers with
      | false ->
         Log.warn (fun f -> f "Invalid GitLab Webhook received!");
         Cohttp_lwt_unix.Server.respond_string ~status:`Unauthorized ~body:"Invalid X-Gitlab-Token" ()
      | true ->
         begin match event with
         | Some "Merge Request Hook" | Some "Push Hook" ->
           begin match Gitlab_j.webhook_of_string body with
            | `MergeRequest _ as x -> Api.input_webhook x
            | `Push _ as x -> Api.input_webhook x
            | x -> Log.warn (fun f -> f "Unknown GitHub event type %S" (Gitlab_j.string_of_webhook x))
           end
         | Some x -> Log.warn (fun f -> f "Unknown GitHub event type %S" x)
         | None -> Log.warn (fun f -> f "Missing GitHub event type in webhook!")
         end;
         Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()
  end
