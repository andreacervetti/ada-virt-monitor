pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Libvirt_Common_Api; use Libvirt_Common_Api;
with Interfaces.C.Strings;

package Libvirt_Host_Api is
   pragma Preelaborate;

   VIR_SECURITY_LABEL_BUFLEN : constant := (4096 + 1);  --  /usr/include/libvirt/libvirt-host.h:85

   VIR_SECURITY_MODEL_BUFLEN : constant := (256 + 1);  --  /usr/include/libvirt/libvirt-host.h:113

   VIR_SECURITY_DOI_BUFLEN : constant := (256 + 1);  --  /usr/include/libvirt/libvirt-host.h:120

   VIR_NODE_CPU_STATS_FIELD_LENGTH : constant := 80;  --  /usr/include/libvirt/libvirt-host.h:177

   VIR_NODE_CPU_STATS_KERNEL : aliased constant String := "kernel" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:194

   VIR_NODE_CPU_STATS_USER : aliased constant String := "user" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:202

   VIR_NODE_CPU_STATS_IDLE : aliased constant String := "idle" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:210

   VIR_NODE_CPU_STATS_IOWAIT : aliased constant String := "iowait" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:218

   VIR_NODE_CPU_STATS_INTR : aliased constant String := "intr" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:226

   VIR_NODE_CPU_STATS_UTILIZATION : aliased constant String := "utilization" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:235

   VIR_NODE_MEMORY_STATS_FIELD_LENGTH : constant := 80;  --  /usr/include/libvirt/libvirt-host.h:255

   VIR_NODE_MEMORY_STATS_TOTAL : aliased constant String := "total" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:272

   VIR_NODE_MEMORY_STATS_FREE : aliased constant String := "free" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:281

   VIR_NODE_MEMORY_STATS_BUFFERS : aliased constant String := "buffers" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:289

   VIR_NODE_MEMORY_STATS_CACHED : aliased constant String := "cached" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:297

   VIR_NODE_MEMORY_SHARED_PAGES_TO_SCAN : aliased constant String := "shm_pages_to_scan" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:318

   VIR_NODE_MEMORY_SHARED_SLEEP_MILLISECS : aliased constant String := "shm_sleep_millisecs" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:326

   VIR_NODE_MEMORY_SHARED_PAGES_SHARED : aliased constant String := "shm_pages_shared" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:334

   VIR_NODE_MEMORY_SHARED_PAGES_SHARING : aliased constant String := "shm_pages_sharing" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:342

   VIR_NODE_MEMORY_SHARED_PAGES_UNSHARED : aliased constant String := "shm_pages_unshared" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:350

   VIR_NODE_MEMORY_SHARED_PAGES_VOLATILE : aliased constant String := "shm_pages_volatile" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:358

   VIR_NODE_MEMORY_SHARED_FULL_SCANS : aliased constant String := "shm_full_scans" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:366

   VIR_NODE_MEMORY_SHARED_MERGE_ACROSS_NODES : aliased constant String := "shm_merge_across_nodes" & ASCII.NUL;  --  /usr/include/libvirt/libvirt-host.h:378
   --  arg-macro: function VIR_NODEINFO_MAXCPUS (nodeinfo)
   --    return (nodeinfo).nodes*(nodeinfo).sockets*(nodeinfo).cores*(nodeinfo).threads;

   VIR_UUID_BUFLEN : constant := (16);  --  /usr/include/libvirt/libvirt-host.h:513

   VIR_UUID_STRING_BUFLEN : constant := (36+1);  --  /usr/include/libvirt/libvirt-host.h:522

   -- * libvirt-host.h
  -- * Summary: APIs for management of hosts
  -- * Description: Provides APIs for the management of hosts
  -- * Author: Daniel Veillard <veillard@redhat.com>
  -- *
  -- * Copyright (C) 2006-2014 Red Hat, Inc.
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
  --  

  --*
  -- * virConnect:
  -- *
  -- * a virConnect is a private structure representing a connection to
  -- * the Hypervisor.
  --  

   --  skipped empty struct u_virConnect

   --  skipped empty struct virConnect

  --*
  -- * virConnectPtr:
  -- *
  -- * a virConnectPtr is pointer to a virConnect private structure, this is the
  -- * type used to reference a connection to the Hypervisor in the API.
  --  

   type virConnectPtr is new System.Address;  -- /usr/include/libvirt/libvirt-host.h:46

  --*
  -- * virNodeSuspendTarget:
  -- *
  -- * Flags to indicate which system-wide sleep state the host must be
  -- * transitioned to.
  --  

  -- This constant is subject to change  
   type virNodeSuspendTarget is 
     (VIR_NODE_SUSPEND_TARGET_MEM,
      VIR_NODE_SUSPEND_TARGET_DISK,
      VIR_NODE_SUSPEND_TARGET_HYBRID);
   pragma Convention (C, virNodeSuspendTarget);  -- /usr/include/libvirt/libvirt-host.h:62

  --*
  -- * virStream:
  -- *
  -- * a virStream is a private structure representing a data stream.
  --  

   --  skipped empty struct u_virStream

   --  skipped empty struct virStream

  --*
  -- * virStreamPtr:
  -- *
  -- * a virStreamPtr is pointer to a virStream private structure, this is the
  -- * type used to reference a data stream in the API.
  --  

   type virStreamPtr is new System.Address;  -- /usr/include/libvirt/libvirt-host.h:77

  --*
  -- * VIR_SECURITY_LABEL_BUFLEN:
  -- *
  -- * Macro providing the maximum length of the virSecurityLabel label string.
  -- * Note that this value is based on that used by Labeled NFS.
  --  

  --*
  -- * virSecurityLabel:
  -- *
  -- * a virSecurityLabel is a structure filled by virDomainGetSecurityLabel(),
  -- * providing the security label and associated attributes for the specified
  -- * domain.
  --  

   type u_virSecurityLabel;
   subtype u_virSecurityLabel_label_array is Interfaces.C.char_array (0 .. 4096);
   subtype virSecurityLabel is u_virSecurityLabel;

  -- security label string  
   type u_virSecurityLabel is record
      label : aliased u_virSecurityLabel_label_array;  -- /usr/include/libvirt/libvirt-host.h:97
      enforcing : aliased int;  -- /usr/include/libvirt/libvirt-host.h:98
   end record;
   pragma Convention (C_Pass_By_Copy, u_virSecurityLabel);  -- /usr/include/libvirt/libvirt-host.h:96

  -- 1 if security policy is being enforced for domain  
  --*
  -- * virSecurityLabelPtr:
  -- *
  -- * a virSecurityLabelPtr is a pointer to a virSecurityLabel.
  --  

   type virSecurityLabelPtr is access all virSecurityLabel;  -- /usr/include/libvirt/libvirt-host.h:106

  --*
  -- * VIR_SECURITY_MODEL_BUFLEN:
  -- *
  -- * Macro providing the maximum length of the virSecurityModel model string.
  --  

  --*
  -- * VIR_SECURITY_DOI_BUFLEN:
  -- *
  -- * Macro providing the maximum length of the virSecurityModel doi string.
  --  

  --*
  -- * virSecurityModel:
  -- *
  -- * a virSecurityModel is a structure filled by virNodeGetSecurityModel(),
  -- * providing the per-hypervisor security model and DOI attributes for the
  -- * specified domain.
  --  

   type u_virSecurityModel;
   subtype u_virSecurityModel_model_array is Interfaces.C.char_array (0 .. 256);
   subtype u_virSecurityModel_doi_array is Interfaces.C.char_array (0 .. 256);
   subtype virSecurityModel is u_virSecurityModel;

  -- security model string  
   type u_virSecurityModel is record
      model : aliased u_virSecurityModel_model_array;  -- /usr/include/libvirt/libvirt-host.h:132
      doi : aliased u_virSecurityModel_doi_array;  -- /usr/include/libvirt/libvirt-host.h:133
   end record;
   pragma Convention (C_Pass_By_Copy, u_virSecurityModel);  -- /usr/include/libvirt/libvirt-host.h:131

  -- domain of interpretation  
  --*
  -- * virSecurityModelPtr:
  -- *
  -- * a virSecurityModelPtr is a pointer to a virSecurityModel.
  --  

   type virSecurityModelPtr is access all virSecurityModel;  -- /usr/include/libvirt/libvirt-host.h:141

  -- data types related to virNodePtr  
  --*
  -- * virNodeInfoPtr:
  -- *
  -- * a virNodeInfo is a structure filled by virNodeGetInfo() and providing
  -- * the information for the Node.
  --  

   type u_virNodeInfo;
   subtype u_virNodeInfo_model_array is Interfaces.C.char_array (0 .. 31);
   subtype virNodeInfo is u_virNodeInfo;

  -- string indicating the CPU model  
   type u_virNodeInfo is record
      model : aliased u_virNodeInfo_model_array;  -- /usr/include/libvirt/libvirt-host.h:156
      memory : aliased unsigned_long;  -- /usr/include/libvirt/libvirt-host.h:157
      cpus : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:158
      mhz : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:159
      nodes : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:161
      sockets : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:164
      cores : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:166
      threads : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:168
   end record;
   pragma Convention (C_Pass_By_Copy, u_virNodeInfo);  -- /usr/include/libvirt/libvirt-host.h:155

  -- memory size in kilobytes  
  -- the number of active CPUs  
  -- expected CPU frequency, 0 if not known or
  --                             on unusual architectures  

  -- the number of NUMA cell, 1 for unusual NUMA
  --                             topologies or uniform memory access; check
  --                             capabilities XML for the actual NUMA topology  

  -- number of CPU sockets per node if nodes > 1,
  --                             1 in case of unusual NUMA topology  

  -- number of cores per socket, total number of
  --                             processors in case of unusual NUMA topology 

  -- number of threads per core, 1 in case of
  --                             unusual numa topology  

  --*
  -- * VIR_NODE_CPU_STATS_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of virNodeCPUStats
  --  

  --*
  -- * VIR_NODE_CPU_STATS_ALL_CPUS:
  -- *
  -- * Value for specifying request for the total CPU time/utilization
  --  

   subtype virNodeGetCPUStatsAllCPUs is unsigned;
   VIR_NODE_CPU_STATS_ALL_CPUS : constant virNodeGetCPUStatsAllCPUs := -1;  -- /usr/include/libvirt/libvirt-host.h:186

  --*
  -- * VIR_NODE_CPU_STATS_KERNEL:
  -- *
  -- * Macro for the cumulative CPU time which was spent by the kernel,
  -- * since the node booting up (in nanoseconds).
  --  

  --*
  -- * VIR_NODE_CPU_STATS_USER:
  -- *
  -- * The cumulative CPU time which was spent by user processes,
  -- * since the node booting up (in nanoseconds).
  --  

  --*
  -- * VIR_NODE_CPU_STATS_IDLE:
  -- *
  -- * The cumulative idle CPU time,
  -- * since the node booting up (in nanoseconds).
  --  

  --*
  -- * VIR_NODE_CPU_STATS_IOWAIT:
  -- *
  -- * The cumulative I/O wait CPU time,
  -- * since the node booting up (in nanoseconds).
  --  

  --*
  -- * VIR_NODE_CPU_STATS_INTR:
  -- *
  -- * The cumulative interrupt CPU time,
  -- * since the node booting up (in nanoseconds).
  --  

  --*
  -- * VIR_NODE_CPU_STATS_UTILIZATION:
  -- *
  -- * The CPU utilization of a node.
  -- * The usage value is in percent and 100% represents all CPUs of
  -- * the node.
  --  

  --*
  -- * virNodeCPUStats:
  -- *
  -- * a virNodeCPUStats is a structure filled by virNodeGetCPUStats()
  -- * providing information about the CPU stats of the node.
  --  

   type u_virNodeCPUStats;
   subtype u_virNodeCPUStats_field_array is Interfaces.C.char_array (0 .. 79);
   subtype virNodeCPUStats is u_virNodeCPUStats;

   type u_virNodeCPUStats is record
      field : aliased u_virNodeCPUStats_field_array;  -- /usr/include/libvirt/libvirt-host.h:246
      value : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-host.h:247
   end record;
   pragma Convention (C_Pass_By_Copy, u_virNodeCPUStats);  -- /usr/include/libvirt/libvirt-host.h:245

  --*
  -- * VIR_NODE_MEMORY_STATS_FIELD_LENGTH:
  -- *
  -- * Macro providing the field length of virNodeMemoryStats
  --  

  --*
  -- * VIR_NODE_MEMORY_STATS_ALL_CELLS:
  -- *
  -- * Value for specifying request for the total memory of all cells.
  --  

   subtype virNodeGetMemoryStatsAllCells is unsigned;
   VIR_NODE_MEMORY_STATS_ALL_CELLS : constant virNodeGetMemoryStatsAllCells := -1;  -- /usr/include/libvirt/libvirt-host.h:264

  --*
  -- * VIR_NODE_MEMORY_STATS_TOTAL:
  -- *
  -- * Macro for the total memory of specified cell:
  -- * it represents the maximum memory.
  --  

  --*
  -- * VIR_NODE_MEMORY_STATS_FREE:
  -- *
  -- * Macro for the free memory of specified cell:
  -- * On Linux, it includes buffer and cached memory, in case of
  -- * VIR_NODE_MEMORY_STATS_ALL_CELLS.
  --  

  --*
  -- * VIR_NODE_MEMORY_STATS_BUFFERS:
  -- *
  -- * Macro for the buffer memory: On Linux, it is only returned in case of
  -- * VIR_NODE_MEMORY_STATS_ALL_CELLS.
  --  

  --*
  -- * VIR_NODE_MEMORY_STATS_CACHED:
  -- *
  -- * Macro for the cached memory: On Linux, it is only returned in case of
  -- * VIR_NODE_MEMORY_STATS_ALL_CELLS.
  --  

  --*
  -- * virNodeMemoryStats:
  -- *
  -- * a virNodeMemoryStats is a structure filled by virNodeGetMemoryStats()
  -- * providing information about the memory of the node.
  --  

   type u_virNodeMemoryStats;
   subtype u_virNodeMemoryStats_field_array is Interfaces.C.char_array (0 .. 79);
   subtype virNodeMemoryStats is u_virNodeMemoryStats;

   type u_virNodeMemoryStats is record
      field : aliased u_virNodeMemoryStats_field_array;  -- /usr/include/libvirt/libvirt-host.h:308
      value : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-host.h:309
   end record;
   pragma Convention (C_Pass_By_Copy, u_virNodeMemoryStats);  -- /usr/include/libvirt/libvirt-host.h:307

  -- * VIR_NODE_MEMORY_SHARED_PAGES_TO_SCAN:
  -- *
  -- * Macro for typed parameter that represents how many present pages
  -- * to scan before the shared memory service goes to sleep.
  --  

  -- * VIR_NODE_MEMORY_SHARED_SLEEP_MILLISECS:
  -- *
  -- * Macro for typed parameter that represents how many milliseconds
  -- * the shared memory service should sleep before next scan.
  --  

  -- * VIR_NODE_MEMORY_SHARED_PAGES_SHARED:
  -- *
  -- * Macro for typed parameter that represents how many the shared
  -- * memory pages are being used.
  --  

  -- * VIR_NODE_MEMORY_SHARED_PAGES_SHARING:
  -- *
  -- * Macro for typed parameter that represents how many sites are
  -- * sharing the pages i.e. how much saved.
  --  

  -- * VIR_NODE_MEMORY_SHARED_PAGES_UNSHARED:
  -- *
  -- * Macro for typed parameter that represents how many pages unique
  -- * but repeatedly checked for merging.
  --  

  -- * VIR_NODE_MEMORY_SHARED_PAGES_VOLATILE:
  -- *
  -- * Macro for typed parameter that represents how many pages changing
  -- * too fast to be placed in a tree.
  --  

  -- * VIR_NODE_MEMORY_SHARED_FULL_SCANS:
  -- *
  -- * Macro for typed parameter that represents how many times all
  -- * mergeable areas have been scanned.
  --  

  -- * VIR_NODE_MEMORY_SHARED_MERGE_ACROSS_NODES:
  -- *
  -- * Macro for typed parameter that represents whether pages from
  -- * different NUMA nodes can be merged. The parameter has type int,
  -- * when its value is 0, only pages which physically reside in the
  -- * memory area of same NUMA node are merged; When its value is 1,
  -- * pages from all nodes can be merged. Other values are reserved
  -- * for future use.
  --  

   function virNodeGetMemoryParameters
     (conn : VirConnectPtr;
      params : virTypedParameterPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:381
   pragma Import (C, virNodeGetMemoryParameters, "virNodeGetMemoryParameters");

   function virNodeSetMemoryParameters
     (conn : VirConnectPtr;
      params : VirTypedParameterPtr;
      nparams : int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:386
   pragma Import (C, virNodeSetMemoryParameters, "virNodeSetMemoryParameters");

  -- *  node CPU map
  --  

   function virNodeGetCPUMap
     (conn : VirConnectPtr;
      cpumap : System.Address;
      online : access unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:394
   pragma Import (C, virNodeGetCPUMap, "virNodeGetCPUMap");

  --*
  -- * VIR_NODEINFO_MAXCPUS:
  -- * @nodeinfo: virNodeInfo instance
  -- *
  -- * This macro is to calculate the total number of CPUs supported
  -- * but not necessary active in the host.
  --  

  --*
  -- * virNodeInfoPtr:
  -- *
  -- * a virNodeInfoPtr is a pointer to a virNodeInfo structure.
  --  

   type virNodeInfoPtr is access all virNodeInfo;  -- /usr/include/libvirt/libvirt-host.h:417

  --*
  -- * virNodeCPUStatsPtr:
  -- *
  -- * a virNodeCPUStatsPtr is a pointer to a virNodeCPUStats structure.
  --  

   type virNodeCPUStatsPtr is access all virNodeCPUStats;  -- /usr/include/libvirt/libvirt-host.h:425

  --*
  -- * virNodeMemoryStatsPtr:
  -- *
  -- * a virNodeMemoryStatsPtr is a pointer to a virNodeMemoryStats structure.
  --  

   type virNodeMemoryStatsPtr is access all virNodeMemoryStats;  -- /usr/include/libvirt/libvirt-host.h:433

  --*
  -- * virConnectFlags
  -- *
  -- * Flags when opening a connection to a hypervisor
  --  

  -- A readonly connection  
  -- Don't try to resolve URI aliases  
   subtype virConnectFlags is unsigned;
   VIR_CONNECT_RO : constant virConnectFlags := 1;
   VIR_CONNECT_NO_ALIASES : constant virConnectFlags := 2;  -- /usr/include/libvirt/libvirt-host.h:443

  -- Identity to act as  
  -- Identify to authorize as  
  -- RFC 1766 languages, comma separated  
  -- client supplies a nonce  
  -- Passphrase secret  
  -- Challenge response  
  -- Challenge response  
  -- Authentication realm  
  -- Externally managed credential  
  -- More may be added - expect the unexpected  
   subtype virConnectCredentialType is unsigned;
   VIR_CRED_USERNAME : constant virConnectCredentialType := 1;
   VIR_CRED_AUTHNAME : constant virConnectCredentialType := 2;
   VIR_CRED_LANGUAGE : constant virConnectCredentialType := 3;
   VIR_CRED_CNONCE : constant virConnectCredentialType := 4;
   VIR_CRED_PASSPHRASE : constant virConnectCredentialType := 5;
   VIR_CRED_ECHOPROMPT : constant virConnectCredentialType := 6;
   VIR_CRED_NOECHOPROMPT : constant virConnectCredentialType := 7;
   VIR_CRED_REALM : constant virConnectCredentialType := 8;
   VIR_CRED_EXTERNAL : constant virConnectCredentialType := 9;  -- /usr/include/libvirt/libvirt-host.h:460

  -- One of virConnectCredentialType constants  
   type u_virConnectCredential is record
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-host.h:463
      prompt : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:464
      challenge : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:465
      defresult : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:466
      result : Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:467
      resultlen : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:468
   end record;
   pragma Convention (C_Pass_By_Copy, u_virConnectCredential);  -- /usr/include/libvirt/libvirt-host.h:462

  -- Prompt to show to user  
  -- Additional challenge to show  
  -- Optional default result  
  -- Result to be filled with user response (or defresult)  
  -- Length of the result  
   subtype virConnectCredential is u_virConnectCredential;

   type virConnectCredentialPtr is access all virConnectCredential;  -- /usr/include/libvirt/libvirt-host.h:472

  --*
  -- * virConnectAuthCallbackPtr:
  -- * @cred: list of virConnectCredential object to fetch from user
  -- * @ncred: size of cred list
  -- * @cbdata: opaque data passed to virConnectOpenAuth
  -- *
  -- * When authentication requires one or more interactions, this callback
  -- * is invoked. For each interaction supplied, data must be gathered
  -- * from the user and filled in to the 'result' and 'resultlen' fields.
  -- * If an interaction cannot be filled, fill in NULL and 0.
  -- *
  -- * Returns 0 if all interactions were filled, or -1 upon error
  --  

   type virConnectAuthCallbackPtr is access function
        (cred : virConnectCredentialPtr;
         ncred : unsigned;
         cbdata : System.Address) return int;
   pragma Convention (C, virConnectAuthCallbackPtr);  -- /usr/include/libvirt/libvirt-host.h:488

  -- List of supported virConnectCredentialType values  
   type u_virConnectAuth is record
      credtype : access int;  -- /usr/include/libvirt/libvirt-host.h:493
      ncredtype : aliased unsigned;  -- /usr/include/libvirt/libvirt-host.h:494
      cb : virConnectAuthCallbackPtr;  -- /usr/include/libvirt/libvirt-host.h:496
      cbdata : System.Address;  -- /usr/include/libvirt/libvirt-host.h:497
   end record;
   pragma Convention (C_Pass_By_Copy, u_virConnectAuth);  -- /usr/include/libvirt/libvirt-host.h:492

  -- Callback used to collect credentials  
   subtype virConnectAuth is u_virConnectAuth;

   type virConnectAuthPtr is access all virConnectAuth;  -- /usr/include/libvirt/libvirt-host.h:502

   virConnectAuthPtrDefault : virConnectAuthPtr;  -- /usr/include/libvirt/libvirt-host.h:504
   pragma Import (C, virConnectAuthPtrDefault, "virConnectAuthPtrDefault");

  --*
  -- * VIR_UUID_BUFLEN:
  -- *
  -- * This macro provides the length of the buffer required
  -- * for virDomainGetUUID()
  --  

  --*
  -- * VIR_UUID_STRING_BUFLEN:
  -- *
  -- * This macro provides the length of the buffer required
  -- * for virDomainGetUUIDString()
  --  

   function virGetVersion
     (libVer : access unsigned_long;
      c_type : Interfaces.C.Strings.chars_ptr;
      typeVer : access unsigned_long) return int;  -- /usr/include/libvirt/libvirt-host.h:525
   pragma Import (C, virGetVersion, "virGetVersion");

  -- * Connection and disconnections to the Hypervisor
  --  

   function virInitialize return int;  -- /usr/include/libvirt/libvirt-host.h:532
   pragma Import (C, virInitialize, "virInitialize");

   function virConnectOpen (name : Interfaces.C.Strings.chars_ptr) return VirConnectPtr;  -- /usr/include/libvirt/libvirt-host.h:534
   pragma Import (C, virConnectOpen, "virConnectOpen");

   function virConnectOpenReadOnly (name : Interfaces.C.Strings.chars_ptr) return VirConnectPtr;  -- /usr/include/libvirt/libvirt-host.h:535
   pragma Import (C, virConnectOpenReadOnly, "virConnectOpenReadOnly");

   function virConnectOpenAuth
     (name : Interfaces.C.Strings.chars_ptr;
      auth : virConnectAuthPtr;
      flags : unsigned) return VirConnectPtr;  -- /usr/include/libvirt/libvirt-host.h:536
   pragma Import (C, virConnectOpenAuth, "virConnectOpenAuth");

   function virConnectRef (conn : VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:539
   pragma Import (C, virConnectRef, "virConnectRef");

   function virConnectClose (conn : VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:540
   pragma Import (C, virConnectClose, "virConnectClose");

   function virConnectGetType (conn : VirConnectPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:541
   pragma Import (C, virConnectGetType, "virConnectGetType");

   function virConnectGetVersion (conn : VirConnectPtr; hvVer : access unsigned_long) return int;  -- /usr/include/libvirt/libvirt-host.h:542
   pragma Import (C, virConnectGetVersion, "virConnectGetVersion");

   function virConnectGetLibVersion (conn : VirConnectPtr; libVer : access unsigned_long) return int;  -- /usr/include/libvirt/libvirt-host.h:544
   pragma Import (C, virConnectGetLibVersion, "virConnectGetLibVersion");

   function virConnectGetHostname (conn : VirConnectPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:546
   pragma Import (C, virConnectGetHostname, "virConnectGetHostname");

   function virConnectGetURI (conn : VirConnectPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:547
   pragma Import (C, virConnectGetURI, "virConnectGetURI");

   function virConnectGetSysinfo (conn : VirConnectPtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:548
   pragma Import (C, virConnectGetSysinfo, "virConnectGetSysinfo");

   function virConnectSetKeepAlive
     (conn : VirConnectPtr;
      interval : int;
      count : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:551
   pragma Import (C, virConnectSetKeepAlive, "virConnectSetKeepAlive");

  --*
  -- * virConnectCloseFunc:
  -- * @conn: virConnect connection
  -- * @reason: reason why the connection was closed (one of virConnectCloseReason)
  -- * @opaque: opaque user data
  -- *
  -- * A callback function to be registered, and called when the connection
  -- * is closed.
  --  

   type virConnectCloseFunc is access procedure
        (conn : VirConnectPtr;
         reason : int;
         opaque : System.Address);
   pragma Convention (C, virConnectCloseFunc);  -- /usr/include/libvirt/libvirt-host.h:563

   function virConnectRegisterCloseCallback
     (conn : VirConnectPtr;
      cb : virConnectCloseFunc;
      opaque : System.Address;
      freecb : virFreeCallback) return int;  -- /usr/include/libvirt/libvirt-host.h:567
   pragma Import (C, virConnectRegisterCloseCallback, "virConnectRegisterCloseCallback");

   function virConnectUnregisterCloseCallback (conn : VirConnectPtr; cb : virConnectCloseFunc) return int;  -- /usr/include/libvirt/libvirt-host.h:571
   pragma Import (C, virConnectUnregisterCloseCallback, "virConnectUnregisterCloseCallback");

  -- * Capabilities of the connection / driver.
  --  

   function virConnectGetMaxVcpus (conn : VirConnectPtr; c_type : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-host.h:578
   pragma Import (C, virConnectGetMaxVcpus, "virConnectGetMaxVcpus");

   function virNodeGetInfo (conn : VirConnectPtr; info : virNodeInfoPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:580
   pragma Import (C, virNodeGetInfo, "virNodeGetInfo");

   function virConnectGetCapabilities (conn : VirConnectPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:582
   pragma Import (C, virConnectGetCapabilities, "virConnectGetCapabilities");

   function virNodeGetCPUStats
     (conn : VirConnectPtr;
      cpuNum : int;
      params : virNodeCPUStatsPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:584
   pragma Import (C, virNodeGetCPUStats, "virNodeGetCPUStats");

   function virNodeGetMemoryStats
     (conn : VirConnectPtr;
      cellNum : int;
      params : virNodeMemoryStatsPtr;
      nparams : access int;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:590
   pragma Import (C, virNodeGetMemoryStats, "virNodeGetMemoryStats");

   function virNodeGetFreeMemory (conn : VirConnectPtr) return Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-host.h:596
   pragma Import (C, virNodeGetFreeMemory, "virNodeGetFreeMemory");

   function virNodeGetSecurityModel (conn : VirConnectPtr; secmodel : virSecurityModelPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:598
   pragma Import (C, virNodeGetSecurityModel, "virNodeGetSecurityModel");

   function virNodeSuspendForDuration
     (conn : VirConnectPtr;
      target : unsigned;
      duration : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:601
   pragma Import (C, virNodeSuspendForDuration, "virNodeSuspendForDuration");

  -- * NUMA support
  --  

   function virNodeGetCellsFreeMemory
     (conn : VirConnectPtr;
      freeMems : access Extensions.unsigned_long_long;
      startCell : int;
      maxCells : int) return int;  -- /usr/include/libvirt/libvirt-host.h:610
   pragma Import (C, virNodeGetCellsFreeMemory, "virNodeGetCellsFreeMemory");

   function virConnectIsEncrypted (conn : VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:616
   pragma Import (C, virConnectIsEncrypted, "virConnectIsEncrypted");

   function virConnectIsSecure (conn : VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:617
   pragma Import (C, virConnectIsSecure, "virConnectIsSecure");

   function virConnectIsAlive (conn : VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-host.h:618
   pragma Import (C, virConnectIsAlive, "virConnectIsAlive");

  -- * CPU specification API
  --  

   subtype virCPUCompareResult is unsigned;
   VIR_CPU_COMPARE_ERROR : constant virCPUCompareResult := -1;
   VIR_CPU_COMPARE_INCOMPATIBLE : constant virCPUCompareResult := 0;
   VIR_CPU_COMPARE_IDENTICAL : constant virCPUCompareResult := 1;
   VIR_CPU_COMPARE_SUPERSET : constant virCPUCompareResult := 2;  -- /usr/include/libvirt/libvirt-host.h:633

  -- treat incompatible
  --                                                             CPUs as failure  

   subtype virConnectCompareCPUFlags is unsigned;
   VIR_CONNECT_COMPARE_CPU_FAIL_INCOMPATIBLE : constant virConnectCompareCPUFlags := 1;  -- /usr/include/libvirt/libvirt-host.h:638

   function virConnectCompareCPU
     (conn : VirConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:640
   pragma Import (C, virConnectCompareCPU, "virConnectCompareCPU");

   function virConnectGetCPUModelNames
     (conn : VirConnectPtr;
      arch : Interfaces.C.Strings.chars_ptr;
      models : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:644
   pragma Import (C, virConnectGetCPUModelNames, "virConnectGetCPUModelNames");

  --*
  -- * virConnectBaselineCPUFlags
  -- *
  -- * Flags when getting XML description of a computed CPU
  --  

  -- show all features  
  -- filter out non-migratable features  
   subtype virConnectBaselineCPUFlags is unsigned;
   VIR_CONNECT_BASELINE_CPU_EXPAND_FEATURES : constant virConnectBaselineCPUFlags := 1;
   VIR_CONNECT_BASELINE_CPU_MIGRATABLE : constant virConnectBaselineCPUFlags := 2;  -- /usr/include/libvirt/libvirt-host.h:657

   function virConnectBaselineCPU
     (conn : VirConnectPtr;
      xmlCPUs : System.Address;
      ncpus : unsigned;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-host.h:659
   pragma Import (C, virConnectBaselineCPU, "virConnectBaselineCPU");

   function virNodeGetFreePages
     (conn : VirConnectPtr;
      npages : unsigned;
      pages : access unsigned;
      startcell : int;
      cellcount : unsigned;
      counts : access Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:665
   pragma Import (C, virNodeGetFreePages, "virNodeGetFreePages");

  -- Add @pageCounts to the pages pool. This
  --                                     can be used only to size up the pool.  

  -- Don't add @pageCounts, instead set
  --                                            passed number of pages. This can be
  --                                            used to free allocated pages.  

   type virNodeAllocPagesFlags is 
     (VIR_NODE_ALLOC_PAGES_ADD,
      VIR_NODE_ALLOC_PAGES_SET);
   pragma Convention (C, virNodeAllocPagesFlags);  -- /usr/include/libvirt/libvirt-host.h:679

   function virNodeAllocPages
     (conn : VirConnectPtr;
      npages : unsigned;
      pageSizes : access unsigned;
      pageCounts : access Extensions.unsigned_long_long;
      startCell : int;
      cellCount : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-host.h:681
   pragma Import (C, virNodeAllocPages, "virNodeAllocPages");

end Libvirt_Host_Api;
