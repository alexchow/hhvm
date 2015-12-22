(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let set_use_watchman _ = ()
let bad_exit _ _ ~is_oom:_ = ()
let init _ _ = ()
let init_monitor _ _ = ()
let init_end _ = ()
let init_really_end _ = ()
let load_mini_worker_end ~is_cached:_ _ _ = ()
let load_mini_state_end _ = ()
let load_read_end _ _ = ()
let load_recheck_end _ _ _ = ()
let load_script_done _ = ()
let load_failed _ = ()
let out_of_date _ = ()
let killed _ = ()
let lock_stolen _ = ()
let client_init _ = ()
let client_check _ _ = ()
let client_build _ _ = ()
let client_start _ = ()
let client_stop _ = ()
let client_restart _ = ()
let client_build_begin_work _ _ = ()
let client_build_finish
  ~rev_changed:_ ~build_type:_ ~request_id:_ ~exit_status:_ = ()
let client_check_finish _ _ _ = ()
let client_bad_exit _ = ()
let monitor_dead_but_typechecker_alive () = ()
let client_established_connection _ = ()
let client_establish_connection_exception _ = ()
let client_connect_once _ = ()
let client_connect_once_busy _ = ()
let check_response _ = ()
let got_client_channels _ =
  Printf.eprintf "Got client channels.\n%!"
let get_client_channels_exception _ =
  Printf.eprintf "Got_client_channels exception\n%!"
let handled_connection _ =
  Printf.eprintf "Handled connection.\n%!"
let handle_connection_exception _ =
  Printf.eprintf "handle_connection exception\n%!"
let build_differs _ _ _ = ()
let build_same _ _ = ()
let recheck_end _ _ _ _ _ = ()
let indexing_end _ = ()
let parsing_end _ _ ~parsed_count:_ = ()
let parsing_hook_end _ = ()
let updating_deps_end _ = ()
let naming_end _ = ()
let type_decl_end _ = ()
let first_redecl_end _ _ = ()
let second_redecl_end _ _ = ()
let type_check_end _ _ = ()
let notifier_returned _ _ = ()
let load_mini_exn _ = ()
let with_id ~stage:_ _ f = f ()
let with_rechecked_stats _ _ _ f = f ()
let with_init_type _ f = f ()

(** Server Monitor events *)
let accepting_on_socket_exception _ = ()
let ack_and_handoff_exception e =
  (Printf.eprintf "ack_and_handoff_exception: %s%!" (Printexc.to_string e);
  Printf.eprintf "%s\n%!" (Printexc.get_backtrace()))
let accepted_client_fd _ = ()
let client_connection_sent _ = ()
let malformed_build_id _ = ()
let send_fd_failure _ = ()
let typechecker_already_running _ = ()
