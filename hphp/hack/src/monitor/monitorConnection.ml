(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ServerMonitorUtils

let from_channel ic =
  (** TODO: Add timeout *)
  Marshal_tools.from_fd_with_preamble (Unix.descr_of_in_channel ic)

let server_exists lock_file = not (Lock.check lock_file)

let wait_on_server_restart ic =
  try
    while true do
      let _ = input_char ic in
      ()
    done
  with
  | End_of_file
  | Sys_error _ ->
     (* Server has exited and hung up on us *)
     ()

let send_build_id_ohai oc =
  Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel oc)
    Build_id.build_id_ohai;
  (** For backwards-compatibility, newline has always followed build id ohai *)
  let _ = Unix.write (Unix.descr_of_out_channel oc) "\n" 0 1 in
  ()


let establish_connection config =
  let sock_name = Socket.get_path config.socket_file in
  let sockaddr =
    if Sys.win32 then
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    else
      Unix.ADDR_UNIX sock_name in
  Result.Ok (Unix.open_connection sockaddr)

let get_cstate (ic, oc) =
  try
    send_build_id_ohai oc;
    let cstate : connection_state = from_channel ic in
    Result.Ok (ic, oc, cstate)
  with e ->
    Unix.shutdown_connection ic;
    close_in_noerr ic;
    raise e

let verify_cstate ic = function
  | Connection_ok -> Result.Ok ()
  | Build_id_mismatch ->
      (* The server is out of date and is going to exit. Subsequent calls
       * to connect on the Unix Domain Socket might succeed, connecting to
       * the server that is about to die, and eventually we will be hung
       * up on while trying to read from our end.
       *
       * To avoid that fate, when we know the server is about to exit, we
       * wait for the connection to be closed, signaling that the server
       * has exited and the OS has cleaned up after it, then we try again.
       *
       * See also: ServerMonitor.client_out_of_date
       *)
      wait_on_server_restart ic;
      close_in_noerr ic;
      Result.Error Build_id_mismatched

(** Consume sequence of Prehandoff messages. *)
let consume_prehandoff_messages ic =
  let open Prehandoff in
  let m: msg = Marshal_tools.from_fd_with_preamble (Unix.descr_of_in_channel ic) in
  match m with
  | Sentinel -> ()
  | Shutting_down ->
    Printf.eprintf "Last server exited. A new will be started.\n%!";
    wait_on_server_restart ic;
    raise Server_shutting_down
  | Server_died {status; was_oom} ->
    (match was_oom, status with
    | true, _ ->
      Printf.eprintf "Last server killed by OOM Manager.\n%!";
    | false, Unix.WEXITED exit_code ->
      Printf.eprintf "Last server exited with code: %d.\n%!" exit_code
    | false, Unix.WSIGNALED signal ->
      Printf.eprintf "Last server killed by signal: %d.\n%!" signal
    | false, Unix.WSTOPPED signal ->
      Printf.eprintf "Last server stopped by signal: %d.\n%!" signal);
    raise Last_server_died

let connect_once config =
  let open Result in
  try
    Sys_utils.with_timeout 1
      ~on_timeout:(fun _ -> raise Exit)
      ~do_:begin fun () ->
        establish_connection config >>= fun (ic, oc) ->
        get_cstate (ic, oc)
      end >>= fun (ic, oc, cstate) ->
      verify_cstate ic cstate >>= fun () ->
      consume_prehandoff_messages ic;
      Printf.eprintf "MonitorConnection Ok\n%!";
      Ok (ic, oc)
  with
  | Exit_status.Exit_with _  as e -> raise e
  | Server_shutting_down ->
    Result.Error Server_missing
  | Last_server_died as e -> raise e
  | _ ->
    if not (server_exists config.lock_file) then Result.Error Server_missing
    else Result.Error Server_busy
