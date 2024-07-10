#!/usr/bin/env bash

# Ensure pipx is in the PATH
if command -v pipx > /dev/null 2>&1; then
  pipx ensurepath

  pipx_packages=("black==21.7b0" "httpie==2.4.0" "flake8==3.9.2")

  # Install or update packages with specific versions
  for pkg in "${pipx_packages[@]}"; do
    pkg_name=$(echo $pkg | cut -d'=' -f1)
    if pipx list | grep -q $pkg_name; then
      pipx upgrade $pkg
    else
      pipx install $pkg
    fi
  done

  # Uninstall packages not in the list
  installed_packages=$(pipx list --short | awk '{print $1}')
  for installed_pkg in $installed_packages; do
    if ! echo "${pipx_packages[@]}" | grep -q $installed_pkg; then
      pipx uninstall $installed_pkg
    fi
  done
else
  echo "pipx is not installed or not in the PATH"
fi
