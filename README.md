# Junks

This is a project with the purpose of allowing me to experiment with different frontend technologies.

In the past I've used it to explore Typescript and Purescript, now I plan to explore Elm and React.

The service is a vanilla note taking/sharing site.

In its current state there is an Elixir/Phoenix backend with a frontend built with Reflex-Dom.

You can see it in action here (defaults to Reflex Dom):
[https://junks.info](https://junks.info)

To access the Reflex Dom frontend visit:
[https://junks.info/rd](https://junks.info/rd)

To access the React Web frontend (in progress) visit:
[https://junks.info/rw](https://junks.info/rw)

<!-- To access the Elm frontend (in progress) visit:
[https://junks.info/rw](https://junks.info/em) -->

## Folders

* `docs` Rough documentation to aid in initial implementation of the backend and a place to keep track of the results of ongoing research.

* `elixir` The Elixir/Phoenix backend. Read about the `mix` commands here [Phoenix Docs](https://hexdocs.pm/phoenix/Phoenix.html)
* `konsoul` Playground for SASS.
* `markdown-editor` A third party Javascript library.
* `material-icons` A third party web graphic resource.
* `materialize` A third party Material Design inspired component library.
* `react-web` The frontend project built with React Web.
* `reflex-dom` The frontend project built with Reflex-Dom.
* `root-assets` Static, top level web assets, like favicon.
* `vagrant` Vagrant setup files.

## Make Commands from the project root

- `clean` Run all *-clean commands.
- `ex-clean` Clean the Elixir build artifacts.
- `rd-clean` Clean the Reflex Dom build artifacts.
- `rd-build` Build the Reflex Dom frontend.
- `rd-dist` Build and move the Reflex Dom artifacts so the Elixir backend can serve them.
- `rw-clean` Clean the React Web build artifacts.
- `rw-build` Build the React Web frontend.
- `rw-dist` Build and move the React Web artifacts so the Elixir backend can serve them.
- `rw-test` Run the React Web tests.
- `m-dist` Build and move the MaterializeCSS artifacts so the Elixir backend can serve them.
- `k-dist` Build and move the Konsoul CSS artifacts so the Elixir backend can serve them.
- `md-dist` Build and move the Markdown Javascript Library artifacts so the Elixir backend can serve them.
- `root-dist` Move the static assets so the Elixir backend can serve them.
- `mi-dist` Move the Material Icons artifacts so the Elixir backend can serve them.
- `assets` Build and distribute all support level static assets so the Elixir backend can serve them. Does not act on Reflex-Dom or React Web.
- `everything` Build and distribute all assets, including support level assets, as well as Reflex-Dom and React Web artifacts.

## Running Locally

### Prerequisites
* Intall vagrant on your machine. [https://www.vagrantup.com/](https://www.vagrantup.com/)

  **NOTE: OSX Time Sync Issue Fix**

  On OSX there can be issues where the VM complains about file times getting out of sync. I've been able to fix this via the following terminal command.

  `VBoxManage guestproperty set {{VM_NAME}} "/VirtualBox/GuestAdd/VBoxService/--timesync-set-threshold" 1000`

* Set up a [Mailgun](https://www.mailgun.com/) account.
* Create the file `elixir/config/keys.exs` using `keys_example.exs` as a template. Set the keys accordingly.

### Let's Play
From the project root.
1. `vagrant up` (if not already running)
2. `vagrant ssh`
3. `cd /src`
4. `make everything`
5. `cd elixir`
6. `mix ecto.create`
7. `mix ecto.migrate`
8. `mix phx.server`
9. Open your browser to [http://172.28.128.200:4000](http://172.28.128.200:4000)


# Moving Forward

My intention is to get the React frontend to be 1-1 with the current Reflex frontend.

After that I plan to flesh out the Elm frontend.
