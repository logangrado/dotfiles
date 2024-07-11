#!/usr/bin/env bash

# Without this, pipx won't be in the path
. /etc/profile.d/nix.sh

pipx_packages=(
  "black 24.4.2"
  "flake8 7.1.0"
)

if command -v pipx > /dev/null 2>&1; then
  echo "Installing requested packages"
  for pkg in "${pipx_packages[@]}"; do
    echo "  $pkg"
    pkg_name=$(echo $pkg | cut -d' ' -f1)
    pkg_version=$(echo $pkg | cut -d' ' -f2)

    if pipx list --short | grep -q "$pkg_name $pkg_version"; then
        echo "    Package already installed"
    else
        pipx install $pkg_name==$pkg_version --force
    fi

  done

  echo "Uninstalling extra packages"
  installed_packages=$(pipx list --short | awk '{print $1}')
  for installed_pkg in $installed_packages; do
    if ! echo "${pipx_packages[@]}" | grep -q $installed_pkg; then
      pipx uninstall $installed_pkg
    fi

  done
  # # Uninstall packages not in the list
  # installed_packages=$(pipx list --short | awk '{print $1}')
  # for installed_pkg in $installed_packages; do
  #   if ! echo "${pipx_packages[@]}" | grep -q $installed_pkg; then
  #     pipx uninstall $installed_pkg
  #   fi
  # done
else
  echo "pipx is not installed or not in the PATH"
  echo $PATH
fi
