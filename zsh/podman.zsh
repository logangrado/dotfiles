_podman_vm_transfer() {
  local action=$1
  shift

  local identity port user remote_command arg
  identity=$(command podman machine inspect --format '{{.SSHConfig.IdentityPath}}') || return
  port=$(command podman machine inspect --format '{{.SSHConfig.Port}}') || return
  user=$(command podman machine inspect --format '{{.SSHConfig.RemoteUsername}}') || return

  remote_command="podman ${(q)action}"
  for arg in "$@"; do
    remote_command+=" ${(q)arg}"
  done

  command ssh -tt \
    -o LogLevel=ERROR \
    -o StrictHostKeyChecking=no \
    -o UserKnownHostsFile=/dev/null \
    -o IdentitiesOnly=yes \
    -i "$identity" \
    -p "$port" \
    "$user@127.0.0.1" \
    "$remote_command"
}

podman() {
  case "$1" in
    pull|push)
      local action=$1
      shift

      # Preserve normal behavior when output is redirected or piped.
      if [[ ! -t 1 ]]; then
        command podman "$action" "$@"
        return
      fi

      _podman_vm_transfer "$action" "$@"
      ;;
    *)
      command podman "$@"
      ;;
  esac
}
