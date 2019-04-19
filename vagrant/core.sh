#!/bin/bash

function postgres_bootstrap
{
  # export DEBIAN_FRONTEND=noninteractive

  sudo apt-get install -y postgresql postgresql-contrib

  sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'postgres';"
}

function nix_bootstrap
{
  # export DEBIAN_FRONTEND=noninteractive

  # install nix
  sh /src/vagrant/install-nix.sh

  # Stage nix configuration
  sudo mkdir -p /etc/nix
  sudo cp /src/vagrant/nix.conf /etc/nix/nix.conf

  # Load profile
  source /home/vagrant/.nix-profile/etc/profile.d/nix.sh

  nix-channel --add https://nixos.org/channels/nixos-18.09 nixos-1809
  nix-channel --update

  # Install cabal tools
  nix-env -iA nixos-1809.cabal-install
  nix-env -iA nixos-1809.cabal2nix
}

function nvm_bootstrap
{
  wget -qO- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash

  . /home/vagrant/.nvm/nvm.sh
}

function node_bootstrap
{
  nvm install node

  npm install -g uglify-js
}

function elm_bootstrap
{
  # export DEBIAN_FRONTEND=noninteractive
  npm install elm -g
  npm install create-elm-app -g
}

function elixir_bootstrap
{
  # export DEBIAN_FRONTEND=noninteractive

  git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.4.3

  echo -e '\n. $HOME/.asdf/asdf.sh' >> ~/.bashrc
  echo -e '\n. $HOME/.asdf/completions/asdf.bash' >> ~/.bashrc

  . /home/vagrant/.asdf/asdf.sh
  asdf plugin-add erlang
  asdf plugin-add elixir

  asdf install erlang 19.3
  asdf install elixir 1.6.4

  asdf global erlang 19.3
  asdf global elixir 1.6.4

  # Phoenix
  mix local.hex --force
  mix local.rebar --force
  mix archive.install https://github.com/phoenixframework/archives/raw/master/phx_new.ez --force
}

function os_bootstrap
{
  # export DEBIAN_FRONTEND=noninteractive

  sudo apt-get update -y

  sudo apt-get install -y unzip automake autoconf libreadline-dev libncurses5-dev libssl-dev libyaml-dev libxslt-dev libffi-dev libtool unixodbc-dev build-essential inotify-tools bash-completion
}

function sass_bootstrap
{
  # export DEBIAN_FRONTEND=noninteractive

  wget https://github.com/sass/dart-sass/releases/download/1.14.1/dart-sass-1.14.1-linux-x64.tar.gz -P ~/
  tar -xvf ~/dart-sass-1.14.1-linux-x64.tar.gz --directory ~/
  sudo mv ~/dart-sass/* /usr/local/bin
  rm -rf ~/dart-sass
  rm ~/dart-sass-1.14.1-linux-x64.tar.gz
}

os_bootstrap
nvm_bootstrap
node_bootstrap
nix_bootstrap
postgres_bootstrap
elixir_bootstrap
sass_bootstrap
elm_bootstrap
