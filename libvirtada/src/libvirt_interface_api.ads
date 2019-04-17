pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with Libvirt_Host_Api;
with Interfaces.C.Strings;

package Libvirt_Interface_Api is
   pragma Preelaborate;

  -- * libvirt-interface.h
  -- * Summary: APIs for management of interfaces
  -- * Description: Provides APIs for the management of interfaces
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
  -- * virInterface:
  -- *
  -- * a virInterface is a private structure representing a virtual interface.
  --  

   --  skipped empty struct u_virInterface

   --  skipped empty struct virInterface

  --*
  -- * virInterfacePtr:
  -- *
  -- * a virInterfacePtr is pointer to a virInterface private structure, this is the
  -- * type used to reference a virtual interface in the API.
  --  

   type virInterfacePtr is new System.Address;  -- /usr/include/libvirt/libvirt-interface.h:44

   function virInterfaceGetConnect (iface : virInterfacePtr) 
                                    return Libvirt_Host_Api.virConnectPtr;  -- /usr/include/libvirt/libvirt-interface.h:46
   pragma Import (C, virInterfaceGetConnect, "virInterfaceGetConnect");

   function virConnectNumOfInterfaces (conn : Libvirt_Host_Api.VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-interface.h:48
   pragma Import (C, virConnectNumOfInterfaces, "virConnectNumOfInterfaces");

   function virConnectListInterfaces
     (conn : Libvirt_Host_Api.VirConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-interface.h:49
   pragma Import (C, virConnectListInterfaces, "virConnectListInterfaces");

   function virConnectNumOfDefinedInterfaces (conn : Libvirt_Host_Api.VirConnectPtr) return int;  -- /usr/include/libvirt/libvirt-interface.h:53
   pragma Import (C, virConnectNumOfDefinedInterfaces, "virConnectNumOfDefinedInterfaces");

   function virConnectListDefinedInterfaces
     (conn : Libvirt_Host_Api.VirConnectPtr;
      names : System.Address;
      maxnames : int) return int;  -- /usr/include/libvirt/libvirt-interface.h:54
   pragma Import (C, virConnectListDefinedInterfaces, "virConnectListDefinedInterfaces");

  -- * virConnectListAllInterfaces:
  -- *
  -- * Flags used to filter the returned interfaces.
  --  

   subtype virConnectListAllInterfacesFlags is unsigned;
   VIR_CONNECT_LIST_INTERFACES_INACTIVE : constant virConnectListAllInterfacesFlags := 1;
   VIR_CONNECT_LIST_INTERFACES_ACTIVE : constant virConnectListAllInterfacesFlags := 2;  -- /usr/include/libvirt/libvirt-interface.h:65

   function virConnectListAllInterfaces
     (conn : Libvirt_Host_Api.VirConnectPtr;
      ifaces : System.Address;
      flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-interface.h:67
   pragma Import (C, virConnectListAllInterfaces, "virConnectListAllInterfaces");

   function virInterfaceLookupByName (conn : Libvirt_Host_Api.VirConnectPtr; name : Interfaces.C.Strings.chars_ptr) return virInterfacePtr;  -- /usr/include/libvirt/libvirt-interface.h:71
   pragma Import (C, virInterfaceLookupByName, "virInterfaceLookupByName");

   function virInterfaceLookupByMACString (conn : Libvirt_Host_Api.VirConnectPtr; mac : Interfaces.C.Strings.chars_ptr) return virInterfacePtr;  -- /usr/include/libvirt/libvirt-interface.h:73
   pragma Import (C, virInterfaceLookupByMACString, "virInterfaceLookupByMACString");

   function virInterfaceGetName (iface : virInterfacePtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-interface.h:76
   pragma Import (C, virInterfaceGetName, "virInterfaceGetName");

   function virInterfaceGetMACString (iface : virInterfacePtr) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-interface.h:77
   pragma Import (C, virInterfaceGetMACString, "virInterfaceGetMACString");

  -- dump inactive interface information  
   subtype virInterfaceXMLFlags is unsigned;
   VIR_INTERFACE_XML_INACTIVE : constant virInterfaceXMLFlags := 1;  -- /usr/include/libvirt/libvirt-interface.h:81

   function virInterfaceGetXMLDesc (iface : virInterfacePtr; flags : unsigned) return Interfaces.C.Strings.chars_ptr;  -- /usr/include/libvirt/libvirt-interface.h:83
   pragma Import (C, virInterfaceGetXMLDesc, "virInterfaceGetXMLDesc");

   function virInterfaceDefineXML
     (conn : Libvirt_Host_Api.VirConnectPtr;
      xmlDesc : Interfaces.C.Strings.chars_ptr;
      flags : unsigned) return virInterfacePtr;  -- /usr/include/libvirt/libvirt-interface.h:85
   pragma Import (C, virInterfaceDefineXML, "virInterfaceDefineXML");

   function virInterfaceUndefine (iface : virInterfacePtr) return int;  -- /usr/include/libvirt/libvirt-interface.h:89
   pragma Import (C, virInterfaceUndefine, "virInterfaceUndefine");

   function virInterfaceCreate (iface : virInterfacePtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-interface.h:91
   pragma Import (C, virInterfaceCreate, "virInterfaceCreate");

   function virInterfaceDestroy (iface : virInterfacePtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-interface.h:94
   pragma Import (C, virInterfaceDestroy, "virInterfaceDestroy");

   function virInterfaceRef (iface : virInterfacePtr) return int;  -- /usr/include/libvirt/libvirt-interface.h:97
   pragma Import (C, virInterfaceRef, "virInterfaceRef");

   function virInterfaceFree (iface : virInterfacePtr) return int;  -- /usr/include/libvirt/libvirt-interface.h:98
   pragma Import (C, virInterfaceFree, "virInterfaceFree");

   function virInterfaceChangeBegin (conn : Libvirt_Host_Api.VirConnectPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-interface.h:100
   pragma Import (C, virInterfaceChangeBegin, "virInterfaceChangeBegin");

   function virInterfaceChangeCommit (conn : Libvirt_Host_Api.VirConnectPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-interface.h:102
   pragma Import (C, virInterfaceChangeCommit, "virInterfaceChangeCommit");

   function virInterfaceChangeRollback (conn : Libvirt_Host_Api.VirConnectPtr; flags : unsigned) return int;  -- /usr/include/libvirt/libvirt-interface.h:104
   pragma Import (C, virInterfaceChangeRollback, "virInterfaceChangeRollback");

   function virInterfaceIsActive (iface : virInterfacePtr) return int;  -- /usr/include/libvirt/libvirt-interface.h:107
   pragma Import (C, virInterfaceIsActive, "virInterfaceIsActive");

end Libvirt_Interface_Api;
