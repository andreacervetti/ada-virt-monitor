# ada-virt-monitor
A virtual machine manager with bindings to libvirt written in Ada

### Table of Contents
* [Work in progress](#work-in-progress)
* [Contacting the author](#contacting-the-author)
* [Compiling](#compiling)
  + [Prerequisites](#prerequisites)
  + [Compilation](#compilation)
* [Structure](#structure)
  + [The libvirt interface](#the-libvirt-interface)
* [Contributing](#contributing)

### Work In Progress
This program and the related libraries are a work in progress, so expect it be
updated often and do not rely on any backward compatibility.

### Contacting the author
Mail me at `ada-virt-monitor<at>homebrewinternet.it`

### Compiling
The program is being developed and tested under ubuntu (18.04 LTS).
It can be compiled and run under FreeBSD also.

#### Prerequisites
ubuntu packages:
- gnat
- gprbuild
- libvirt-dev
- libgtkada16.1.0-dev
- libxmlada development libraries (`libxmlada-*-dev`)
- libncursesada5-dev (needed only to compile the test program `linemonitor.adb`.
If you don't need it remove the program and the dependency from `avm.gpr`)
- libvirt0
- netcat
- ssk-askpass
- any dependency from the packages above

#### Compilation
Go to directory `avm`.

Run `gprbuild -P avm.gpr`.

### Structure
The basic idea is to define an engine ("monitor") managing a dynamic sructure
with the information related to defined hypervisor and their status.
A task periodically upgrades the informations (status of virtual machines,
statistics, etc.)

Hypervisors are divided into groups, so that different kind of hypervisors can
be keep separated.

A default group is ever present.

The user interface interacts with the monitor through the functions in the
library. Different user interfaces could be developed.

Currently the interface is a gtkada one (directory gui).

A simpler textual interface is the program linemonitor in directory tests.

#### The libvirt interface
The files `libvirt_*_api.ads` are binding to libvirt C function. They are
compiler-generated with some tweaking.

The files `virtada-*` are a partial thick binding to libvirt.

They define Ada tagged types and methods to interact with libvirt.

#### Contributing
Contributions in any form (suggestions, criticisms, code ...) are welcome.

We only ask you to follow some simple guidelines.

- Keep the library at Ada 2005 syntax.
- Do not interface the monitor with libvirt C functions. Use virtada instead
- Do not interface the UI with virtada. Interface with the monitor.

If you are interested in in being added to contributor list mail us at the
address above.
