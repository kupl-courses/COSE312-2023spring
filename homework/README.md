# Setup

1. Install VirtualBox and Vagrant

2. Create and run a virtual machine: 
```
vagrant up
vagrant ssh
```

3. Inside the virtual machine, run the following commands:
```
sudo add-apt-repository ppa:deadsnakes/ppa -y
sudo apt install python3.10 -y
```

4. Build a project:
```
cd develop
cd hw2/regex2dfa
dune build main.exe
```
