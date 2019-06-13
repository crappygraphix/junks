Vagrant.configure('2') do |config|
  config.vm.box = "bento/ubuntu-16.04"
  config.vm.hostname = 'junks-dev'
  config.vm.network 'private_network', ip: '172.28.128.200'

  config.vm.provider 'virtualbox' do |v|
    v.memory = 4096
    v.cpus = 2
    v.customize ["modifyvm", :id, "--audio", "none"]
    v.customize ["modifyvm", :id, "--vrde", "off"]
    v.customize ["guestproperty", "set", :id, "--timesync-threshold", 1000]
  end
  
  config.vm.provider 'hyperv' do |h, override|
    h.memory = 4096
    h.maxmemory = 6144
    h.cpus = 2

    override.vm.synced_folder '.', '/src', type: 'smb', mount_options: ['vers=3.0'], smb_host: ENV['VAGRANT_SMB_HOST'], smb_username: ENV['VAGRANT_SMB_USERNAME']
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
