pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with Libvirt_Host_Api; use Libvirt_Host_Api;
with Libvirt_Domain_Api; use Libvirt_Domain_Api;
with Libvirt_Network_Api; use Libvirt_Network_Api;

package Virterror is
   pragma Preelaborate;

  -- * virterror.h: Error handling interfaces for the libvirt library
  -- * Summary: error handling interfaces for the libvirt library
  -- * Description: Provides the interfaces of the libvirt library to handle
  -- *              errors raised while using the library.
  -- *
  -- * Copyright (C) 2006-2016 Red Hat, Inc.
  -- *
  -- * This library is free software; you can redistribute it and/or
  -- * modify it under the terms of the GNU Lesser General Public
  -- * License as published by the Free Software Foundation; either
  -- * version 2.1 of the License, or (at your option) any later version.
  -- *
  -- * This library is distributed in the hope that it will be useful,
  -- * but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  -- * Lesser General Public License for more details.
  -- *
  -- * You should have received a copy of the GNU Lesser General Public
  -- * License along with this library.  If not, see
  -- * <http://www.gnu.org/licenses/>.
  -- *
  -- * Author: Daniel Veillard <veillard@redhat.com>
  --  

  --*
  -- * virErrorLevel:
  -- *
  -- * Indicates the level of an error
  --  

  -- A simple warning  
  -- An error  
   type virErrorLevel is 
     (VIR_ERR_NONE,
      VIR_ERR_WARNING,
      VIR_ERR_ERROR);
   pragma Convention (C, virErrorLevel);  -- /usr/include/libvirt/virterror.h:44

  --*
  -- * virErrorDomain:
  -- *
  -- * Indicates where an error may have come from.  This should remain
  -- * stable, with all additions placed at the end since libvirt 0.1.0.
  --  

   type virErrorDomain is 
     (VIR_FROM_NONE,
      VIR_FROM_XEN,             -- Error at Xen hypervisor layer  
      VIR_FROM_XEND,            -- Error at connection with xend daemon 
      VIR_FROM_XENSTORE,        -- Error at connection with xen store  
      VIR_FROM_SEXPR,           -- Error in the S-Expression code  
      VIR_FROM_XML,             -- Error in the XML code  
      VIR_FROM_DOM,             -- Error when operating on a domain  
      VIR_FROM_RPC,             -- Error in the XML-RPC code 
      VIR_FROM_PROXY,           -- Error in the proxy code; unused since 0.8.6  
      VIR_FROM_CONF,            -- Error in the configuration file handling  
      VIR_FROM_QEMU,            -- Error at the QEMU daemon  
      VIR_FROM_NET,             -- Error when operating on a network  
      VIR_FROM_TEST,            -- Error from test driver  
      VIR_FROM_REMOTE,          -- Error from remote driver  
      VIR_FROM_OPENVZ,          -- Error from OpenVZ driver  
      VIR_FROM_XENXM,           -- Error at Xen XM layer  
      VIR_FROM_STATS_LINUX,     -- Error in the Linux Stats code  
      VIR_FROM_LXC,             -- Error from Linux Container driver  
      VIR_FROM_STORAGE,         -- Error from storage driver  
      VIR_FROM_NETWORK,         -- Error from network config  
      VIR_FROM_DOMAIN,          -- Error from domain config  
      VIR_FROM_UML,             -- Error at the UML driver  
      VIR_FROM_NODEDEV,         -- Error from node device monitor  
      VIR_FROM_XEN_INOTIFY,     -- Error from xen inotify layer  
      VIR_FROM_SECURITY,        -- Error from security framework  
      VIR_FROM_VBOX,            -- Error from VirtualBox driver  
      VIR_FROM_INTERFACE,       -- Error when operating on an interface  
      VIR_FROM_ONE,             -- The OpenNebula driver no longer exists.
      --                           Retained for ABI/API compat only  
      VIR_FROM_ESX,             -- Error from ESX driver  
      VIR_FROM_PHYP,            -- Error from IBM power hypervisor  
      VIR_FROM_SECRET,          -- Error from secret storage  
      VIR_FROM_CPU,             -- Error from CPU driver  
      VIR_FROM_XENAPI,          -- Error from XenAPI  
      VIR_FROM_NWFILTER,        -- Error from network filter driver  
      VIR_FROM_HOOK,            -- Error from Synchronous hooks  
      VIR_FROM_DOMAIN_SNAPSHOT, -- Error from domain snapshot  
      VIR_FROM_AUDIT,           -- Error from auditing subsystem  
      VIR_FROM_SYSINFO,         -- Error from sysinfo/SMBIOS  
      VIR_FROM_STREAMS,         -- Error from I/O streams  
      VIR_FROM_VMWARE,          -- Error from VMware driver  
      VIR_FROM_EVENT,           -- Error from event loop impl  
      VIR_FROM_LIBXL,           -- Error from libxenlight driver  
      VIR_FROM_LOCKING,         -- Error from lock manager  
      VIR_FROM_HYPERV,          -- Error from Hyper-V driver  
      VIR_FROM_CAPABILITIES,    -- Error from capabilities  
      VIR_FROM_URI,             -- Error from URI handling  
      VIR_FROM_AUTH,            -- Error from auth handling  
      VIR_FROM_DBUS,            -- Error from DBus  
      VIR_FROM_PARALLELS,       -- Error from Parallels  
      VIR_FROM_DEVICE,          -- Error from Device  
      VIR_FROM_SSH,             -- Error from libssh2 connection transport  
      VIR_FROM_LOCKSPACE,       -- Error from lockspace  
      VIR_FROM_INITCTL,         -- Error from initctl device communication  
      VIR_FROM_IDENTITY,        -- Error from identity code  
      VIR_FROM_CGROUP,          -- Error from cgroups  
      VIR_FROM_ACCESS,          -- Error from access control manager  
      VIR_FROM_SYSTEMD,         -- Error from systemd code  
      VIR_FROM_BHYVE,           -- Error from bhyve driver  
      VIR_FROM_CRYPTO,          -- Error from crypto code  
      VIR_FROM_FIREWALL,        -- Error from firewall  
      VIR_FROM_POLKIT,          -- Error from polkit code  
      VIR_FROM_THREAD,          -- Error from thread utils  
      VIR_FROM_ADMIN,           -- Error from admin backend  
      VIR_FROM_LOGGING,         -- Error from log manager  
      VIR_FROM_XENXL,           -- Error from Xen xl config code  
      VIR_FROM_PERF,            -- Error from perf  
      VIR_FROM_LIBSSH           -- Error from libssh connection transport  
     );
   pragma Convention (C, virErrorDomain);  -- /usr/include/libvirt/virterror.h:139

  --*
  -- * virError:
  -- *
  -- * A libvirt Error instance.
  -- *
  -- * The conn, dom and net fields should be used with extreme care.
  -- * Reference counts are not incremented so the underlying objects
  -- * may be deleted without notice after the error has been delivered.
  --  

   type u_virError;
   subtype virError is u_virError;

   type virErrorPtr is new System.Address;  -- /usr/include/libvirt/virterror.h:153

  -- The error code, a virErrorNumber  
   type u_virError is record
      code : aliased int;  -- /usr/include/libvirt/virterror.h:155
      domain : aliased int;  -- /usr/include/libvirt/virterror.h:156
      message : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/virterror.h:157
      level : aliased virErrorLevel;  -- /usr/include/libvirt/virterror.h:158
      conn : virConnectPtr;  -- /usr/include/libvirt/virterror.h:159
      dom : virDomainPtr;  -- /usr/include/libvirt/virterror.h:161
      str1 : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/virterror.h:163
      str2 : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/virterror.h:164
      str3 : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/virterror.h:165
      int1 : aliased int;  -- /usr/include/libvirt/virterror.h:166
      int2 : aliased int;  -- /usr/include/libvirt/virterror.h:167
      net : virNetworkPtr;  -- /usr/include/libvirt/virterror.h:168
   end record;
   pragma Convention (C_Pass_By_Copy, u_virError);  -- /usr/include/libvirt/virterror.h:154

  -- What part of the library raised this error  
  -- human-readable informative error message  
  -- how consequent is the error  
  -- connection if available, deprecated
  --                                          see note above  

  -- domain if available, deprecated
  --                                        see note above  

  -- extra string information  
  -- extra string information  
  -- extra string information  
  -- extra number information  
  -- extra number information  
  -- network if available, deprecated
  --                                         see note above  

  --*
  -- * virErrorNumber:
  -- *
  -- * The full list of errors the library can generate
  -- *
  -- * This list should remain stable, with all additions placed at the
  -- * end since libvirt 0.1.0.  There is one exception: values added
  -- * between libvirt 0.7.1 and libvirt 0.7.7 (VIR_WAR_NO_SECRET through
  -- * VIR_ERR_MIGRATE_PERSIST_FAILED) were inadvertently relocated by
  -- * four positions in 0.8.0.  If you suspect version mismatch between a
  -- * server and client, then you can decipher values as follows:
  -- *
  -- * switch (err.code) {
  -- * case 60:
  -- *     // no way to tell VIR_WAR_NO_SECRET apart from VIR_WAR_NO_NWFILTER,
  -- *     // but both are very similar warnings
  -- *     break;
  -- * case 61: case 62: case 63:
  -- *     if (err.domain != VIR_FROM_NWFILTER)
  -- *         err.code += 4;
  -- *     break;
  -- * case 64:
  -- *     if (err.domain == VIR_FROM_QEMU)
  -- *         err.code += 4;
  -- *     break;
  -- * case 65:
  -- *     if (err.domain == VIR_FROM_XEN)
  -- *         err.code += 4;
  -- *     break;
  -- * default:
  -- * }
  --  




   type virErrorNumber is 
     (VIR_ERR_OK,
      VIR_ERR_INTERNAL_ERROR,          -- internal error  
      VIR_ERR_NO_MEMORY,               -- memory allocation failure  
      VIR_ERR_NO_SUPPORT,              -- no support for this function  
      VIR_ERR_UNKNOWN_HOST,            -- could not resolve hostname  
      VIR_ERR_NO_CONNECT,              -- can't connect to hypervisor  
      VIR_ERR_INVALID_CONN,            -- invalid connection object  
      VIR_ERR_INVALID_DOMAIN,          -- invalid domain object  
      VIR_ERR_INVALID_ARG,             -- invalid function argument  
      VIR_ERR_OPERATION_FAILED,        -- a command to hypervisor failed  
      VIR_ERR_GET_FAILED,              -- a HTTP GET command to failed  
      VIR_ERR_POST_FAILED,             -- a HTTP POST command to failed  
      VIR_ERR_HTTP_ERROR,              -- unexpected HTTP error code  
      VIR_ERR_SEXPR_SERIAL,            -- failure to serialize an S-Expr  
      VIR_ERR_NO_XEN,                  -- could not open Xen hypervisor control  
      VIR_ERR_XEN_CALL,                -- failure doing an hypervisor call  
      VIR_ERR_OS_TYPE,                 -- unknown OS type  
      VIR_ERR_NO_KERNEL,               -- missing kernel information  
      VIR_ERR_NO_ROOT,                 -- missing root device information  
      VIR_ERR_NO_SOURCE,               -- missing source device information  
      VIR_ERR_NO_TARGET,               -- missing target device information  
      VIR_ERR_NO_NAME,                 -- missing domain name information  
      VIR_ERR_NO_OS,                   -- missing domain OS information  
      VIR_ERR_NO_DEVICE,               -- missing domain devices information  
      VIR_ERR_NO_XENSTORE,             -- could not open Xen Store control  
      VIR_ERR_DRIVER_FULL,             -- too many drivers registered  
      VIR_ERR_CALL_FAILED,             -- not supported by the drivers
      --                                  (DEPRECATED)  
      VIR_ERR_XML_ERROR,               -- an XML description is not well formed
      --                                  or broken  
      VIR_ERR_DOM_EXIST,               -- the domain already exist  
      VIR_ERR_OPERATION_DENIED,        -- operation forbidden on read-only
      --                                  connections  
      VIR_ERR_OPEN_FAILED,             -- failed to open a conf file  
      VIR_ERR_READ_FAILED,             -- failed to read a conf file  
      VIR_ERR_PARSE_FAILED,            -- failed to parse a conf file  
      VIR_ERR_CONF_SYNTAX,             -- failed to parse the syntax of a
      --                                  conf file  
      VIR_ERR_WRITE_FAILED,            -- failed to write a conf file  
      VIR_ERR_XML_DETAIL,              -- detail of an XML error  
      VIR_ERR_INVALID_NETWORK,         -- invalid network object  
      VIR_ERR_NETWORK_EXIST,           -- the network already exist  
      VIR_ERR_SYSTEM_ERROR,            -- general system call failure  
      VIR_ERR_RPC,                     -- some sort of RPC error  
      VIR_ERR_GNUTLS_ERROR,            -- error from a GNUTLS call  
      VIR_WAR_NO_NETWORK,              -- failed to start network  
      VIR_ERR_NO_DOMAIN,               -- domain not found or unexpectedly
      --                                  disappeared  
      VIR_ERR_NO_NETWORK,              -- network not found  
      VIR_ERR_INVALID_MAC,             -- invalid MAC address  
      VIR_ERR_AUTH_FAILED,             -- authentication failed  
      VIR_ERR_INVALID_STORAGE_POOL,    -- invalid storage pool object  
      VIR_ERR_INVALID_STORAGE_VOL,     -- invalid storage vol object  
      VIR_WAR_NO_STORAGE,              -- failed to start storage  
      VIR_ERR_NO_STORAGE_POOL,         -- storage pool not found  
      VIR_ERR_NO_STORAGE_VOL,          -- storage volume not found  
      VIR_WAR_NO_NODE,                 -- failed to start node driver  
      VIR_ERR_INVALID_NODE_DEVICE,     -- invalid node device object  
      VIR_ERR_NO_NODE_DEVICE,          -- node device not found  
      VIR_ERR_NO_SECURITY_MODEL,       -- security model not found  
      VIR_ERR_OPERATION_INVALID,       -- operation is not applicable at this
      --                                  time  
      VIR_WAR_NO_INTERFACE,            -- failed to start interface driver  
      VIR_ERR_NO_INTERFACE,            -- interface driver not running  
      VIR_ERR_INVALID_INTERFACE,       -- invalid interface object  
      VIR_ERR_MULTIPLE_INTERFACES,     -- more than one matching interface
      --                                  found  
      VIR_WAR_NO_NWFILTER,             -- failed to start nwfilter driver  
      VIR_ERR_INVALID_NWFILTER,        -- invalid nwfilter object  
      VIR_ERR_NO_NWFILTER,             -- nw filter pool not found  
      VIR_ERR_BUILD_FIREWALL,          -- nw filter pool not found  
      VIR_WAR_NO_SECRET,               -- failed to start secret storage  
      VIR_ERR_INVALID_SECRET,          -- invalid secret  
      VIR_ERR_NO_SECRET,               -- secret not found  
      VIR_ERR_CONFIG_UNSUPPORTED,      -- unsupported configuration
      --                                  construct  
      VIR_ERR_OPERATION_TIMEOUT,       -- timeout occurred during operation  
      VIR_ERR_MIGRATE_PERSIST_FAILED,  -- a migration worked, but making the
      --                                  VM persist on the dest host failed  
      VIR_ERR_HOOK_SCRIPT_FAILED,      -- a synchronous hook script failed  
      VIR_ERR_INVALID_DOMAIN_SNAPSHOT, -- invalid domain snapshot  
      VIR_ERR_NO_DOMAIN_SNAPSHOT,      -- domain snapshot not found  
      VIR_ERR_INVALID_STREAM,          -- stream pointer not valid  
      VIR_ERR_ARGUMENT_UNSUPPORTED,    -- valid API use but unsupported by
      --                                  the given driver  
      VIR_ERR_STORAGE_PROBE_FAILED,    -- storage pool probe failed  
      VIR_ERR_STORAGE_POOL_BUILT,      -- storage pool already built  
      VIR_ERR_SNAPSHOT_REVERT_RISKY,   -- force was not requested for a
      --                                  risky domain snapshot revert  
      VIR_ERR_OPERATION_ABORTED,       -- operation on a domain was
      --                                  canceled/aborted by user  
      VIR_ERR_AUTH_CANCELLED,          -- authentication cancelled  
      VIR_ERR_NO_DOMAIN_METADATA,      -- The metadata is not present  
      VIR_ERR_MIGRATE_UNSAFE,          -- Migration is not safe  
      VIR_ERR_OVERFLOW,                -- integer overflow  
      VIR_ERR_BLOCK_COPY_ACTIVE,       -- action prevented by block copy job  
      VIR_ERR_OPERATION_UNSUPPORTED,   -- The requested operation is not
      --                                  supported  
      VIR_ERR_SSH,                     -- error in ssh transport driver  
      VIR_ERR_AGENT_UNRESPONSIVE,      -- guest agent is unresponsive,
      --                                  not running or not usable  
      VIR_ERR_RESOURCE_BUSY,           -- resource is already in use  
      VIR_ERR_ACCESS_DENIED,           -- operation on the object/resource
      --                                  was denied  
      VIR_ERR_DBUS_SERVICE,            -- error from a dbus service  
      VIR_ERR_STORAGE_VOL_EXIST,       -- the storage vol already exists  
      VIR_ERR_CPU_INCOMPATIBLE,        -- given CPU is incompatible with host
      --                                  CPU 
      VIR_ERR_XML_INVALID_SCHEMA,      -- XML document doesn't validate against schema  
      VIR_ERR_MIGRATE_FINISH_OK,       -- Finish API succeeded but it is expected to return NULL  
      VIR_ERR_AUTH_UNAVAILABLE,        -- authentication unavailable  
      VIR_ERR_NO_SERVER,               -- Server was not found  
      VIR_ERR_NO_CLIENT,               -- Client was not found  
      VIR_ERR_AGENT_UNSYNCED,          -- guest agent replies with wrong id
      --                                  to guest-sync command  
      VIR_ERR_LIBSSH                   -- error in libssh transport driver  
     );
   pragma Convention (C, virErrorNumber);  -- /usr/include/libvirt/virterror.h:322

  --*
  -- * virErrorFunc:
  -- * @userData:  user provided data for the error callback
  -- * @error:  the error being raised.
  -- *
  -- * Signature of a function to use when there is an error raised by the library.
  --  

   type virErrorFunc is access procedure (userData : System.Address; error : virErrorPtr);
   pragma Convention (C, virErrorFunc);  -- /usr/include/libvirt/virterror.h:331

  -- * Errors can be handled as asynchronous callbacks or after the routine
  -- * failed. They can also be handled globally at the library level, or
  -- * at the connection level (which then has priority).
  --  

   function VirGetLastError return VirErrorPtr;  -- /usr/include/libvirt/virterror.h:339
   pragma Import (C, virGetLastError, "virGetLastError");

   function virSaveLastError return virErrorPtr;  -- /usr/include/libvirt/virterror.h:340
   pragma Import (C, virSaveLastError, "virSaveLastError");

   procedure virResetLastError;  -- /usr/include/libvirt/virterror.h:341
   pragma Import (C, virResetLastError, "virResetLastError");

   procedure virResetError (err : virErrorPtr);  -- /usr/include/libvirt/virterror.h:342
   pragma Import (C, virResetError, "virResetError");

   procedure virFreeError (err : virErrorPtr);  -- /usr/include/libvirt/virterror.h:343
   pragma Import (C, virFreeError, "virFreeError");

   function virGetLastErrorMessage return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/virterror.h:345
   pragma Import (C, virGetLastErrorMessage, "virGetLastErrorMessage");

   function virConnGetLastError (conn : virConnectPtr) return virErrorPtr;  -- /usr/include/libvirt/virterror.h:347
   pragma Import (C, virConnGetLastError, "virConnGetLastError");

   procedure virConnResetLastError (conn : virConnectPtr);  -- /usr/include/libvirt/virterror.h:348
   pragma Import (C, virConnResetLastError, "virConnResetLastError");

   function virCopyLastError (to : virErrorPtr) return int;  -- /usr/include/libvirt/virterror.h:349
   pragma Import (C, virCopyLastError, "virCopyLastError");

   procedure virDefaultErrorFunc (err : virErrorPtr);  -- /usr/include/libvirt/virterror.h:351
   pragma Import (C, virDefaultErrorFunc, "virDefaultErrorFunc");

   procedure virSetErrorFunc (userData : System.Address; handler : virErrorFunc);  -- /usr/include/libvirt/virterror.h:352
   pragma Import (C, virSetErrorFunc, "virSetErrorFunc");

   procedure virConnSetErrorFunc
     (conn : virConnectPtr;
      userData : System.Address;
      handler : virErrorFunc);  -- /usr/include/libvirt/virterror.h:354
   pragma Import (C, virConnSetErrorFunc, "virConnSetErrorFunc");

   function virConnCopyLastError (conn : virConnectPtr; to : virErrorPtr) return int;  -- /usr/include/libvirt/virterror.h:357
   pragma Import (C, virConnCopyLastError, "virConnCopyLastError");

end Virterror;
