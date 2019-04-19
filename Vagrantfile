Vagrant.configure('2') do |config|
  config.vm.box = "bento/ubuntu-16.04"
  config.vm.hostname = 'junks-dev'
  config.vm.network 'private_network', ip: '172.28.128.200'

  config.vm.provider 'virtualbox' do |v|
    v.memory = 4096
    v.cpus = 2
  end

  config.vm.synced_folder '.', '/vagrant', disabled: true
  config.vm.synced_folder '.', '/src'

  config.vm.provision 'shell',
    path: 'vagrant/core.sh',
    privileged: false,
    binary: true

  config.vm.network 'forwarded_port',
    guest: 5623,
    host: 5623
end
