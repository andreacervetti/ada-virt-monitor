pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with Libvirt_Host_Api;  use Libvirt_Host_Api;
with Libvirt_Common_Api; use Libvirt_Common_Api;

package Libvirt_Storage_Api is

   --  arg-macro: function VIR_STORAGE_POOL_EVENT_CALLBACK (cb)
   --    return (virConnectStoragePoolEventGenericCallback)(cb);
   --  skipped empty struct u_virStoragePool

   --  skipped empty struct virStoragePool

   type virStoragePoolPtr is new System.Address;  -- /usr/include/libvirt/libvirt-storage.h:45

   type virStoragePoolState is 
     (VIR_STORAGE_POOL_INACTIVE,
      VIR_STORAGE_POOL_BUILDING,
      VIR_STORAGE_POOL_RUNNING,
      VIR_STORAGE_POOL_DEGRADED,
      VIR_STORAGE_POOL_INACCESSIBLE);
   pragma Convention (C, virStoragePoolState);  -- /usr/include/libvirt/libvirt-storage.h:58

   subtype virStoragePoolBuildFlags is unsigned;
   VIR_STORAGE_POOL_BUILD_NEW : constant virStoragePoolBuildFlags := 0;
   VIR_STORAGE_POOL_BUILD_REPAIR : constant virStoragePoolBuildFlags := 1;
   VIR_STORAGE_POOL_BUILD_RESIZE : constant virStoragePoolBuildFlags := 2;
   VIR_STORAGE_POOL_BUILD_NO_OVERWRITE : constant virStoragePoolBuildFlags := 4;
   VIR_STORAGE_POOL_BUILD_OVERWRITE : constant virStoragePoolBuildFlags := 8;  -- /usr/include/libvirt/libvirt-storage.h:66

   type virStoragePoolDeleteFlags is 
     (VIR_STORAGE_POOL_DELETE_NORMAL,
      VIR_STORAGE_POOL_DELETE_ZEROED);
   pragma Convention (C, virStoragePoolDeleteFlags);  -- /usr/include/libvirt/libvirt-storage.h:71

   subtype virStoragePoolCreateFlags is unsigned;
   VIR_STORAGE_POOL_CREATE_NORMAL : constant virStoragePoolCreateFlags := 0;
   VIR_STORAGE_POOL_CREATE_WITH_BUILD : constant virStoragePoolCreateFlags := 1;
   VIR_STORAGE_POOL_CREATE_WITH_BUILD_OVERWRITE : constant virStoragePoolCreateFlags := 2;
   VIR_STORAGE_POOL_CREATE_WITH_BUILD_NO_OVERWRITE : constant virStoragePoolCreateFlags := 4;  -- /usr/include/libvirt/libvirt-storage.h:88

   type u_virStoragePoolInfo;
   subtype virStoragePoolInfo is u_virStoragePoolInfo;  -- /usr/include/libvirt/libvirt-storage.h:90

   type u_virStoragePoolInfo is record
      state : aliased int;  -- /usr/include/libvirt/libvirt-storage.h:93
      capacity : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-storage.h:94
      allocation : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-storage.h:95
      available : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-storage.h:96
   end record;
   pragma Convention (C_Pass_By_Copy, u_virStoragePoolInfo);  -- /usr/include/libvirt/libvirt-storage.h:92

   type virStoragePoolInfoPtr is access all virStoragePoolInfo;  -- /usr/include/libvirt/libvirt-storage.h:99

   --  skipped empty struct u_virStorageVol

   --  skipped empty struct virStorageVol

   type virStorageVolPtr is new System.Address;  -- /usr/include/libvirt/libvirt-storage.h:115

   type virStorageVolType is 
     (VIR_STORAGE_VOL_FILE,
      VIR_STORAGE_VOL_BLOCK,
      VIR_STORAGE_VOL_DIR,
      VIR_STORAGE_VOL_NETWORK,
      VIR_STORAGE_VOL_NETDIR,
      VIR_STORAGE_VOL_PLOOP);
   pragma Convention (C, virStorageVolType);  -- /usr/include/libvirt/libvirt-storage.h:130

   type virStorageVolDeleteFlags is 
     (VIR_STORAGE_VOL_DELETE_NORMAL,
      VIR_STORAGE_VOL_DELETE_ZEROED,
      VIR_STORAGE_VOL_DELETE_WITH_SNAPSHOTS);
   pragma Convention (C, virStorageVolDeleteFlags);  -- /usr/include/libvirt/libvirt-storage.h:136

   type virStorageVolWipeAlgorithm is 
     (VIR_STORAGE_VOL_WIPE_ALG_ZERO,
      VIR_STORAGE_VOL_WIPE_ALG_NNSA,
      VIR_STORAGE_VOL_WIPE_ALG_DOD,
      VIR_STORAGE_VOL_WIPE_ALG_BSI,
      VIR_STORAGE_VOL_WIPE_ALG_GUTMANN,
      VIR_STORAGE_VOL_WIPE_ALG_SCHNEIER,
      VIR_STORAGE_VOL_WIPE_ALG_PFITZNER7,
      VIR_STORAGE_VOL_WIPE_ALG_PFITZNER33,
      VIR_STORAGE_VOL_WIPE_ALG_RANDOM,
      VIR_STORAGE_VOL_WIPE_ALG_TRIM);
   pragma Convention (C, virStorageVolWipeAlgorithm);  -- /usr/include/libvirt/libvirt-storage.h:168

   type virStorageVolInfoFlags is 
     (VIR_STORAGE_VOL_USE_ALLOCATION,
      VIR_STORAGE_VOL_GET_PHYSICAL);
   pragma Convention (C, virStorageVolInfoFlags);  -- /usr/include/libvirt/libvirt-storage.h:176

   type u_virStorageVolInfo;
   subtype virStorageVolInfo is u_virStorageVolInfo;  -- /usr/include/libvirt/libvirt-storage.h:178

   type u_virStorageVolInfo is record
      c_type : aliased int;  -- /usr/include/libvirt/libvirt-storage.h:181
      capacity : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-storage.h:182
      allocation : aliased Extensions.unsigned_long_long;  -- /usr/include/libvirt/libvirt-storage.h:183
   end record;
   pragma Convention (C_Pass_By_Copy, u_virStorageVolInfo);  -- /usr/include/libvirt/libvirt-storage.h:180

   type virStorageVolInfoPtr is access all virStorageVolInfo;  -- /usr/include/libvirt/libvirt-storage.h:186

   subtype virStorageXMLFlags is unsigned;
   VIR_STORAGE_XML_INACTIVE : constant virStorageXMLFlags := 1;  -- /usr/include/libvirt/libvirt-storage.h:190

   function virStoragePoolGetConnect (pool : virStoragePoolPtr) return virConnectPtr;  -- /usr/include/libvirt/libvirt-storage.h:195
   pragma Import (C, virStoragePoolGetConnect, "virStoragePoolGetConnect");

   function virConnectNumOfStoragePools (conn : virConnectPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:200
   pragma Import (C, virConnectNumOfStoragePools, "virConnectNumOfStoragePools");

   function virConnectListStoragePools
     (conn : virConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-storage.h:201
   pragma Import (C, virConnectListStoragePools, "virConnectListStoragePools");

   function virConnectNumOfDefinedStoragePools (conn : virConnectPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:208
   pragma Import (C, virConnectNumOfDefinedStoragePools, "virConnectNumOfDefinedStoragePools");

   function virConnectListDefinedStoragePools
     (conn : virConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-storage.h:209
   pragma Import (C, virConnectListDefinedStoragePools, "virConnectListDefinedStoragePools");

   subtype virConnectListAllStoragePoolsFlags is unsigned;
   VIR_CONNECT_LIST_STORAGE_POOLS_INACTIVE : constant virConnectListAllStoragePoolsFlags := 1;
   VIR_CONNECT_LIST_STORAGE_POOLS_ACTIVE : constant virConnectListAllStoragePoolsFlags := 2;
   VIR_CONNECT_LIST_STORAGE_POOLS_PERSISTENT : constant virConnectListAllStoragePoolsFlags := 4;
   VIR_CONNECT_LIST_STORAGE_POOLS_TRANSIENT : constant virConnectListAllStoragePoolsFlags := 8;
   VIR_CONNECT_LIST_STORAGE_POOLS_AUTOSTART : constant virConnectListAllStoragePoolsFlags := 16;
   VIR_CONNECT_LIST_STORAGE_POOLS_NO_AUTOSTART : constant virConnectListAllStoragePoolsFlags := 32;
   VIR_CONNECT_LIST_STORAGE_POOLS_DIR : constant virConnectListAllStoragePoolsFlags := 64;
   VIR_CONNECT_LIST_STORAGE_POOLS_FS : constant virConnectListAllStoragePoolsFlags := 128;
   VIR_CONNECT_LIST_STORAGE_POOLS_NETFS : constant virConnectListAllStoragePoolsFlags := 256;
   VIR_CONNECT_LIST_STORAGE_POOLS_LOGICAL : constant virConnectListAllStoragePoolsFlags := 512;
   VIR_CONNECT_LIST_STORAGE_POOLS_DISK : constant virConnectListAllStoragePoolsFlags := 1024;
   VIR_CONNECT_LIST_STORAGE_POOLS_ISCSI : constant virConnectListAllStoragePoolsFlags := 2048;
   VIR_CONNECT_LIST_STORAGE_POOLS_SCSI : constant virConnectListAllStoragePoolsFlags := 4096;
   VIR_CONNECT_LIST_STORAGE_POOLS_MPATH : constant virConnectListAllStoragePoolsFlags := 8192;
   VIR_CONNECT_LIST_STORAGE_POOLS_RBD : constant virConnectListAllStoragePoolsFlags := 16384;
   VIR_CONNECT_LIST_STORAGE_POOLS_SHEEPDOG : constant virConnectListAllStoragePoolsFlags := 32768;
   VIR_CONNECT_LIST_STORAGE_POOLS_GLUSTER : constant virConnectListAllStoragePoolsFlags := 65536;
   VIR_CONNECT_LIST_STORAGE_POOLS_ZFS : constant virConnectListAllStoragePoolsFlags := 131072;
   VIR_CONNECT_LIST_STORAGE_POOLS_VSTORAGE : constant virConnectListAllStoragePoolsFlags := 262144;  -- /usr/include/libvirt/libvirt-storage.h:244

   function virConnectListAllStoragePools
     (conn : virConnectPtr;
      pools : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:246
   pragma Import (C, virConnectListAllStoragePools, "virConnectListAllStoragePools");

   function virConnectFindStoragePoolSources
     (conn : virConnectPtr;
      c_type : Interfaces.C.Strings.chars_ptr;
      srcSpec : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:252
   pragma Import (C, virConnectFindStoragePoolSources, "virConnectFindStoragePoolSources");

   function virStoragePoolLookupByName (conn : virConnectPtr; name : Interfaces.C.Strings.chars_ptr) return virStoragePoolPtr;  -- /usr/include/libvirt/libvirt-storage.h:260
   pragma Import (C, virStoragePoolLookupByName, "virStoragePoolLookupByName");

   function virStoragePoolLookupByUUID (conn : virConnectPtr; uuid : access unsigned_char) return virStoragePoolPtr;  -- /usr/include/libvirt/libvirt-storage.h:262
   pragma Import (C, virStoragePoolLookupByUUID, "virStoragePoolLookupByUUID");

   function virStoragePoolLookupByUUIDString (conn : virConnectPtr; uuid : Interfaces.C.Strings.chars_ptr) return virStoragePoolPtr;  -- /usr/include/libvirt/libvirt-storage.h:264
   pragma Import (C, virStoragePoolLookupByUUIDString, "virStoragePoolLookupByUUIDString");

   function virStoragePoolLookupByVolume (vol : virStorageVolPtr) return virStoragePoolPtr;  -- /usr/include/libvirt/libvirt-storage.h:266
   pragma Import (C, virStoragePoolLookupByVolume, "virStoragePoolLookupByVolume");

   function virStoragePoolCreateXML
     (conn : virConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virStoragePoolPtr;  -- /usr/include/libvirt/libvirt-storage.h:271
   pragma Import (C, virStoragePoolCreateXML, "virStoragePoolCreateXML");

   function virStoragePoolDefineXML
     (conn : virConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virStoragePoolPtr;  -- /usr/include/libvirt/libvirt-storage.h:274
   pragma Import (C, virStoragePoolDefineXML, "virStoragePoolDefineXML");

   function virStoragePoolBuild (pool : virStoragePoolPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:277
   pragma Import (C, virStoragePoolBuild, "virStoragePoolBuild");

   function virStoragePoolUndefine (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:279
   pragma Import (C, virStoragePoolUndefine, "virStoragePoolUndefine");

   function virStoragePoolCreate (pool : virStoragePoolPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:280
   pragma Import (C, virStoragePoolCreate, "virStoragePoolCreate");

   function virStoragePoolDestroy (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:282
   pragma Import (C, virStoragePoolDestroy, "virStoragePoolDestroy");

   function virStoragePoolDelete (pool : virStoragePoolPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:283
   pragma Import (C, virStoragePoolDelete, "virStoragePoolDelete");

   function virStoragePoolRef (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:285
   pragma Import (C, virStoragePoolRef, "virStoragePoolRef");

   function virStoragePoolFree (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:286
   pragma Import (C, virStoragePoolFree, "virStoragePoolFree");

   function virStoragePoolRefresh (pool : virStoragePoolPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:287
   pragma Import (C, virStoragePoolRefresh, "virStoragePoolRefresh");

   function virStoragePoolGetName (pool : virStoragePoolPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:293
   pragma Import (C, virStoragePoolGetName, "virStoragePoolGetName");

   function virStoragePoolGetUUID (pool : virStoragePoolPtr; uuid : access unsigned_char) return int;  -- /usr/include/libvirt/libvirt-storage.h:294
   pragma Import (C, virStoragePoolGetUUID, "virStoragePoolGetUUID");

   function virStoragePoolGetUUIDString (pool : virStoragePoolPtr; buf : Interfaces.C.Strings.chars_ptr) return int;  -- /usr/include/libvirt/libvirt-storage.h:296
   pragma Import (C, virStoragePoolGetUUIDString, "virStoragePoolGetUUIDString");

   function virStoragePoolGetInfo (vol : virStoragePoolPtr; info : virStoragePoolInfoPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:299
   pragma Import (C, virStoragePoolGetInfo, "virStoragePoolGetInfo");

   function virStoragePoolGetXMLDesc (pool : virStoragePoolPtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:302
   pragma Import (C, virStoragePoolGetXMLDesc, "virStoragePoolGetXMLDesc");

   function virStoragePoolGetAutostart (pool : virStoragePoolPtr; autostart : access int) return int;  -- /usr/include/libvirt/libvirt-storage.h:305
   pragma Import (C, virStoragePoolGetAutostart, "virStoragePoolGetAutostart");

   function virStoragePoolSetAutostart (pool : virStoragePoolPtr; autostart : int) return int;  -- /usr/include/libvirt/libvirt-storage.h:307
   pragma Import (C, virStoragePoolSetAutostart, "virStoragePoolSetAutostart");

   function virStoragePoolNumOfVolumes (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:313
   pragma Import (C, virStoragePoolNumOfVolumes, "virStoragePoolNumOfVolumes");

   function virStoragePoolListVolumes
     (pool : virStoragePoolPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-storage.h:314
   pragma Import (C, virStoragePoolListVolumes, "virStoragePoolListVolumes");

   function virStoragePoolListAllVolumes
     (pool : virStoragePoolPtr;
      vols : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:317
   pragma Import (C, virStoragePoolListAllVolumes, "virStoragePoolListAllVolumes");

   function virStorageVolGetConnect (vol : virStorageVolPtr) return virConnectPtr;  -- /usr/include/libvirt/libvirt-storage.h:321
   pragma Import (C, virStorageVolGetConnect, "virStorageVolGetConnect");

   function virStorageVolLookupByName (pool : virStoragePoolPtr; name : Interfaces.C.Strings.chars_ptr) return virStorageVolPtr;  -- /usr/include/libvirt/libvirt-storage.h:326
   pragma Import (C, virStorageVolLookupByName, "virStorageVolLookupByName");

   function virStorageVolLookupByKey (conn : virConnectPtr; key : Interfaces.C.Strings.chars_ptr) return virStorageVolPtr;  -- /usr/include/libvirt/libvirt-storage.h:328
   pragma Import (C, virStorageVolLookupByKey, "virStorageVolLookupByKey");

   function virStorageVolLookupByPath (conn : virConnectPtr; path : Interfaces.C.Strings.chars_ptr) return virStorageVolPtr;  -- /usr/include/libvirt/libvirt-storage.h:330
   pragma Import (C, virStorageVolLookupByPath, "virStorageVolLookupByPath");

   function virStorageVolGetName (vol : virStorageVolPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:334
   pragma Import (C, virStorageVolGetName, "virStorageVolGetName");

   function virStorageVolGetKey (vol : virStorageVolPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:335
   pragma Import (C, virStorageVolGetKey, "virStorageVolGetKey");

   subtype virStorageVolCreateFlags is unsigned;
   VIR_STORAGE_VOL_CREATE_PREALLOC_METADATA : constant virStorageVolCreateFlags := 1;
   VIR_STORAGE_VOL_CREATE_REFLINK : constant virStorageVolCreateFlags := 2;  -- /usr/include/libvirt/libvirt-storage.h:340

   function virStorageVolCreateXML
     (pool : virStoragePoolPtr;
      xmldesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virStorageVolPtr;  -- /usr/include/libvirt/libvirt-storage.h:342
   pragma Import (C, virStorageVolCreateXML, "virStorageVolCreateXML");

   function virStorageVolCreateXMLFrom
     (pool : virStoragePoolPtr;
      xmldesc : Interfaces.C.Strings.chars_ptr;
      clonevol : virStorageVolPtr;
      flags : unsigned) return virStorageVolPtr;  -- /usr/include/libvirt/libvirt-storage.h:345
   pragma Import (C, virStorageVolCreateXMLFrom, "virStorageVolCreateXMLFrom");

   subtype virStorageVolDownloadFlags is unsigned;
   VIR_STORAGE_VOL_DOWNLOAD_SPARSE_STREAM : constant virStorageVolDownloadFlags := 1;  -- /usr/include/libvirt/libvirt-storage.h:352

   function virStorageVolDownload
     (vol : virStorageVolPtr;
      stream : virStreamPtr;
      offset : Extensions.unsigned_long_long;
      length : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:354
   pragma Import (C, virStorageVolDownload, "virStorageVolDownload");

   subtype virStorageVolUploadFlags is unsigned;
   VIR_STORAGE_VOL_UPLOAD_SPARSE_STREAM : constant virStorageVolUploadFlags := 1;  -- /usr/include/libvirt/libvirt-storage.h:361

   function virStorageVolUpload
     (vol : virStorageVolPtr;
      stream : virStreamPtr;
      offset : Extensions.unsigned_long_long;
      length : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:363
   pragma Import (C, virStorageVolUpload, "virStorageVolUpload");

   function virStorageVolDelete (vol : virStorageVolPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:368
   pragma Import (C, virStorageVolDelete, "virStorageVolDelete");

   function virStorageVolWipe (vol : virStorageVolPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:370
   pragma Import (C, virStorageVolWipe, "virStorageVolWipe");

   function virStorageVolWipePattern
     (vol : virStorageVolPtr;
      algorithm : unsigned;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:372
   pragma Import (C, virStorageVolWipePattern, "virStorageVolWipePattern");

   function virStorageVolRef (vol : virStorageVolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:375
   pragma Import (C, virStorageVolRef, "virStorageVolRef");

   function virStorageVolFree (vol : virStorageVolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:376
   pragma Import (C, virStorageVolFree, "virStorageVolFree");

   function virStorageVolGetInfo (vol : virStorageVolPtr; info : virStorageVolInfoPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:378
   pragma Import (C, virStorageVolGetInfo, "virStorageVolGetInfo");

   function virStorageVolGetInfoFlags
     (vol : virStorageVolPtr;
      info : virStorageVolInfoPtr;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:380
   pragma Import (C, virStorageVolGetInfoFlags, "virStorageVolGetInfoFlags");

   function virStorageVolGetXMLDesc (pool : virStorageVolPtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:383
   pragma Import (C, virStorageVolGetXMLDesc, "virStorageVolGetXMLDesc");

   function virStorageVolGetPath (vol : virStorageVolPtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-storage.h:386
   pragma Import (C, virStorageVolGetPath, "virStorageVolGetPath");

   subtype virStorageVolResizeFlags is unsigned;
   VIR_STORAGE_VOL_RESIZE_ALLOCATE : constant virStorageVolResizeFlags := 1;
   VIR_STORAGE_VOL_RESIZE_DELTA : constant virStorageVolResizeFlags := 2;
   VIR_STORAGE_VOL_RESIZE_SHRINK : constant virStorageVolResizeFlags := 4;  -- /usr/include/libvirt/libvirt-storage.h:392

   function virStorageVolResize
     (vol : virStorageVolPtr;
      capacity : Extensions.unsigned_long_long;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-storage.h:394
   pragma Import (C, virStorageVolResize, "virStorageVolResize");

   function virStoragePoolIsActive (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:398
   pragma Import (C, virStoragePoolIsActive, "virStoragePoolIsActive");

   function virStoragePoolIsPersistent (pool : virStoragePoolPtr) return int;  -- /usr/include/libvirt/libvirt-storage.h:399
   pragma Import (C, virStoragePoolIsPersistent, "virStoragePoolIsPersistent");

   type virStoragePoolEventID is 
     (VIR_STORAGE_POOL_EVENT_ID_LIFECYCLE,
      VIR_STORAGE_POOL_EVENT_ID_REFRESH);
   pragma Convention (C, virStoragePoolEventID);  -- /usr/include/libvirt/libvirt-storage.h:428

   type virConnectStoragePoolEventGenericCallback is access procedure
        (conn   : virConnectPtr;
         pool   : virStoragePoolPtr;
         opaque : System.Address);
   pragma Convention (C, virConnectStoragePoolEventGenericCallback);  -- /usr/include/libvirt/libvirt-storage.h:442

   function virConnectStoragePoolEventRegisterAny
     (conn : virConnectPtr;
      pool : virStoragePoolPtr;
      eventID : int;
      cb : virConnectStoragePoolEventGenericCallback;
      opaque : System.Address;
      freecb : virFreeCallback) return int;  -- /usr/include/libvirt/libvirt-storage.h:447
   pragma Import (C, virConnectStoragePoolEventRegisterAny, "virConnectStoragePoolEventRegisterAny");

   function virConnectStoragePoolEventDeregisterAny (conn : virConnectPtr; callbackID : int) return int;  -- /usr/include/libvirt/libvirt-storage.h:454
   pragma Import (C, virConnectStoragePoolEventDeregisterAny, "virConnectStoragePoolEventDeregisterAny");

   type virStoragePoolEventLifecycleType is 
     (VIR_STORAGE_POOL_EVENT_DEFINED,
      VIR_STORAGE_POOL_EVENT_UNDEFINED,
      VIR_STORAGE_POOL_EVENT_STARTED,
      VIR_STORAGE_POOL_EVENT_STOPPED,
      VIR_STORAGE_POOL_EVENT_CREATED,
      VIR_STORAGE_POOL_EVENT_DELETED);
   pragma Convention (C, virStoragePoolEventLifecycleType);  -- /usr/include/libvirt/libvirt-storage.h:474

   type virConnectStoragePoolEventLifecycleCallback is access procedure
        (conn : virConnectPtr;
         pool : virStoragePoolPtr;
         event : int;
         detail : int;
         opaque : System.Address);
   pragma Convention (C, virConnectStoragePoolEventLifecycleCallback);  -- /usr/include/libvirt/libvirt-storage.h:491

end Libvirt_Storage_Api;
